# gbfs Web UI API 契約 (設計 v1)

Status: 設計段階 (未実装)。`gbfs.Server` に追加する API 契約を定義する。
方針: **gbfs.Lib の実データ形状をそのまま標準化する** — Desktop (Avalonia) で実機検証済みの
frame / audio / input の形を Web 側も同じにする。Lib を変えることを前提にしない。

## 設計判断 (3案)

**推奨: サーササイドセッション + WebSocket でフレーム/音声を配信、制御は REST。**

| # | 案 | 判定 |
|---|---|---|
| **1 (推奨)** | **gbfs.Server 内にエミュレータセッションを保持。実行 (60fps) はサーバ、フレーム+音声+状態を WebSocket で配信、ROM 読込/入力は REST POST** | クライアントは表示のみで良い。複数タブ/複数クライアントで同一セッションを観覧可能。MCP relay (既存 POST/GET) とセッションを同一 store に統合でき、将来「MCP が操作している画面をブラウザが観覧」も同一経路になる |
| 2 | クライアント主導: ブラウザが `POST /step` で1フレーム実行し、フレームを受け取る (REST polling) | 却下: ブラウザのタイマー精度に依存し 60fps 実行が破綻しやすい。往復レイテンシも音声に致命的 |
| 3 | Lib を WASM に載せてブラウザ内で実行 (Bolero 再演) | 却下: 今回の移行理由 (JS interop/WASM の複雑さ) に逆行する。AI (MCP) と画面を共有できない |

### 決めたこと (案1 の内訳)

- **実行主体はサーバ**: `DispatcherTimer` 相当 (ASP.NET の PeriodicTimer) で60fps。ブラウザは受け取って描くだけ
- **データプレーンは WebSocket 1本**: frame (binary) + audio (binary) + state (text JSON) を同一接続で
- **コントロールプレーンは REST**: ROM アップロード、start/stop/reset、button press/release、state snapshot
- **セッションは v1 で単一 (default)**。パスは `/api/sessions/{id?}` を予約せず単純ルートで開始し、マルチセッション化するときに `/api/v1/...` へ versioning する

## データ形式 (Lib 実測準拠 — 検証済み)

| 名前 | 形式 | 出所 |
|---|---|---|
| **frame** | `byte[23040]` (160×144)。値 0-3 = 4 shade palette index | `Emulator.getFrameBuffer` / 既存 relay と同じ |
| **palette** | 0: `#9BBC0F`, 1: `#8BAC0F`, 2: `#306230`, 3: `#0F380F` (緑系) | Desktop `Program.fs` の `palette` と共通。クライアント側で RGBA 化 |
| **audio** | `float32` L/R interleaved、44100Hz。1フレーム ≈ 735フレーム (1470 float) | `Emulator.getAudioBuffer` (interleaved L/R、実測 maxAbs=0.25) |
| **button** | `"up" \| "down" \| "left" \| "right" \| "a" \| "b" \| "start" \| "select"` | `Joypad.pressButton` の文字列と一致 (未知値は無視) |

## REST (コントロールプレーン) — `http://localhost:5032`

### `POST /api/rom`
ROM をアップロードしてセッションにロード (既存状態はリセット)。
- Request: `application/octet-stream` (ROM バイト列) 或いは multipart file
- Response: `204 No Content`
- 失敗: `400` + `{"error": "..."}`

### `POST /api/session`
セッションの実行制御。
- Request: `{"action": "start" | "stop" | "reset"}`
- Response: `204`
- `reset` は ROM を保持した状態でエンジン初期化 (`Emulator.reset`)。

### `POST /api/button`
- Request: `{"buttons": ["a", "b"], "action": "press" | "release"}`
- Response: `204`
- 実装: `Emulator.pressButton` / `releaseButton` を順に呼ぶ。未知ボタンは無視 (Lib 仕様どおり)。

### `GET /api/state`
現在のセッション状態 (polling 用 snapshot。UI のステータス表示・デバッグ用)。
- Response: `200 application/json`
```json
{
  "romLoaded": true,
  "running": true,
  "frameCount": 1234,
  "totalCycles": 53952000,
  "pc": "0x0157",
  "sp": "0xFFFE",
  "registers": { "af": "0x01B0", "bc": "0x0013", "...": "..." },
  "flags": { "z": false, "n": false, "h": false, "c": false },
  "halted": false
}
```

### 既存 relay (互換維持)
- `POST /api/mcp/frame` / `GET /api/mcp/frame` — **変更しない**。MCP の FrameRelay はそのまま動く。
- v1 ではセッション frame と relay frame は別バッファ (統合は v2 の課題。#2 設計判断の「将来統合」を参照)。

## WebSocket (データプレーン) — `ws://localhost:5032/ws`

接続後にサーバが60fpsで push。クライアントからの送信は使わない (入力は REST のため)。

| 種別 | payload | 形式 |
|---|---|---|
| **binary** メッセージ | **frame** | `byte[23040]`。直近フレーム 1枚。値 0-3 |
| **binary** メッセージ | **audio** | `float32[]` L/R interleaved (直近分のサンプル、≈1470 float) |
| **text** メッセージ | **state** | `GET /api/state` と同じ JSON。1秒に1回程度、または frameCount 変化時 |

フレームと音声は **同一 WS で送るがメッセージ単位で分離**する (binary は frame か audio かを
判別するための 1-byte ヘッダを付けるか、別メッセージで送るかを実装時に決める — 推奨:
**frame = `0x01` prefix + 23040 bytes、audio = `0x02` prefix + float32 bytes** の1バイトタグ方式)。

クライアント→サーバのメッセージ: なし (v1)。入力は REST (`POST /api/button`)。
ボタンの連打レイテンシが問題になった場合のみ、v2 で WS 上の入力メッセージを追加する。

## クライアント実装の指針 (Web UI 側)

- 描画: `<canvas width=160 height=144>` + `ImageData` に palette index → RGBA 変換。
  Desktop と同じく最近傍で拡大 (`image-rendering: pixelated`)
- 音声: `AudioWorklet` に float32 を流し ring buffer。44100Hz 固定
- 入力: keydown/keyup → debounce なしで `POST /api/button` (押下/解放の各1回)
- ROM 選択: `<input type=file>` → `fetch POST /api/rom`

## 実装ステップ (見積り)

1. **Server にセッション状態 + REST 4本** (`/api/rom`, `/api/session`, `/api/button`, `/api/state`) — Lib は変更不要。~60分
2. **WS `/ws` + 60fps サーバループ** (frame/audio 配信) — ~60分
3. **Web クライアント (canvas + AudioWorklet + 入力)** — `wwwroot` に追加。~90分
4. **統合テスト**: relay 互換 (MCP 動作不変) + Web 動作 + `dotnet test` 224件維持 — ~30分

v1 実装合計: ~4時間 (Lib 変更なし)。

## 未決事項 (v2)

- **セッション frame と MCP relay frame の統合**: 「AI が操作している画面をブラウザが観覧する」
  パターンでは両者を1つの store にまとめる必要がある。v1 では別バッファで互換を優先。
- マルチセッション (複数 ROM 並行)、セッションの寿命管理 (idle で停止)
- WS 入力メッセージ (レイテンシ最適化)
- 認証: v1 は localhost/LAN 専用を想定。公開暴露時は必須 (proxy 側の basic auth で十分)
