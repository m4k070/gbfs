= CGB (Game Boy Color) 対応
:toc:

gbfs はマシン種別として DMG と CGB を持つ (`Memory.MachineMode`)。
本ドキュメントは「CGB の起動時状態」までを扱い、KEY1/倍速・パレット・VRAM バンクは未実装の項目として残す。

== マシン種別とモード

* `Memory.MachineMode = Dmg | Cgb` は **ハードウェアの種別** (どの実機か)。
* カートリッジ側の CGB 対応はヘッダ `0x0143` bit7 で示される。
  CGB マシンでも `0x0143` bit7 = 0 のカートは **DMG 互換モード**で動き、
  CGB 専用レジスタ (`0xFF4C`/`0xFF4D`/`0xFF4F`/`0xFF51-0xFF56`/`0xFF68-0xFF6B`/`0xFF70`/`0xFF72-0xFF77`) は
  `0xFF` を読む (Pan Docs: CGB Registers)。
* 既定は DMG (`Decoder.createState ()`)。CGB は `Decoder.createStateFor Memory.Cgb ()`。
  テスト ROM ランナーは `--machine dmg|cgb` で切り替える。

mooneye の CGB 用テスト ROM (`misc/` の `-C` / `cgb` 系) はヘッダ `0x0143 = 0x00` なので、
**CGB ハード上の DMG 互換モード**として走る。したがって `--machine cgb` で回す。

== CGB モードの解禁 (CgbMode)

`MemoryBus.CgbMode` は **マシンが CGB かつ ヘッダ `0x0143` bit7 = 1** のときだけ true になる
(ROM ロード時に確定)。Pan Docs: CGB Registers の
"you must first unlock CGB features by changing byte 0143" に対応する。

* true のとき: CGB 専用レジスタが機能し、VRAM/WRAM のバンク切り替えが有効になる
* false のとき: CGB 専用レジスタは **`0xFF` を読み、書き込みは無視**される。
  CGB モードの起動値 (KEY1 = 0x7E, OPRI = 0xFE, SVBK の読み出し = 0xF8, FF74 = 0x00) も
  `Memory.loadRom` で設定する。これらはブート ROM が書き込む値なので検証 ROM が無い (**未検証**)

=== メモリバンク

[cols="1,1,1,3"]
|===
| レジスタ | 名称 | 読み出し | 動作

| 0xFF4F | VBK | `0xFE \| bank` | VRAM バンク (0/1) を切り替える。非 CGB モードでは無視
| 0xFF70 | SVBK | `0xF8 \| value` | D000-DFFF の WRAM バンク。0 はバンク 1 として扱う。非 CGB モードでは無視
|===

* VRAM は 2 バンク 16KB。バンク 0 = タイル/タイルマップ、バンク 1 = タイルマップの属性 (CGB)。
  **PPU は `Memory.readVramBank0` で常にバンク 0 を読む** (CPU の VBK とは独立)。
  CGB の属性 (パレット/反転/優先度) を読む処理は未実装
* WRAM は 8 バンク 32KB。C000-CFFF はバンク 0 固定、D000-DFFF は SVBK で切り替え。
  Echo RAM (E000-FDFF) も同じバンクを見る
* 非 CGB モードの SVBK は値 0x00 のまま扱われ、実効バンクは 1 固定

== KEY0 / KEY1 / OPRI / 未定義レジスタ

出典: Pan Docs "CGB Registers"。

[cols="1,1,4"]
|===
| アドレス | 名称 | 挙動

| 0xFF4C | KEY0 | 非 CGB モード: `0xFF`。CGB モード: `0xFE` (bit0 = DMG 互換モード = 0)。
  ブート ROM 完了後にロックされるため**書き込みは無視**する
| 0xFF4D | KEY1 | bit7 = 現在の速度 (読み出し専用)、bit0 = 速度切替のアーム (読み書き)。
  CGB モードの読み出しは `0x7E \| bit7 \| bit0`、非 CGB モードは `0xFF`
| 0xFF6C | OPRI | bit0 = オブジェクト優先度 (0 = CGB 方式, 1 = DMG 方式)。非 CGB モードは `0xFF`
| 0xFF74 | 未定義 | CGB モードでは読み書き可 (初期 0x00)、非 CGB モードは `0xFF` 固定
| 0xFF75 | 未定義 | bit4-6 のみ読み書き可 (`0x8F \| (value & 0x70)`)。両モード共通
|===

== STOP と倍速 (Double Speed)

Pan Docs "CGB Registers / KEY1" の手順: KEY1 の bit0 を立ててから STOP を実行すると
速度が切り替わり、bit0 は自動でクリアされる。切替後は **2050 M サイクル = 8200 T サイクル**
CPU が停止する。停止中は DIV が進まない。

* 倍速で 2 倍になる: CPU、タイマ/分周器、シリアル、OAM DMA
* 等速のまま: LCD、APU、HDMA
* 実装: `Decoder.stepPeripherals` が PPU と APU に `cycles / 2` を渡し、
  タイマには `cycles` をそのまま渡す (gbfs の命令サイクルはすべて 4 の倍数なので割り切れる)
* 停止は `CpuState.Stopped` に残りサイクルを保持し、停止中は命令を実行せずタイマも進めない
* **倍速判定は `CgbMode` でゲートすること**。非 CGB モードでは `0xFF4D` が `0xFF` を読むため、
  Io の生値だけを見ると bit7 が立っており倍速と誤判定する (実装中に実際に踏んだ回帰)
* 未検証: 停止中の割込みの扱い、シリアル/OAM DMA の倍速化 (Pan Docs に TODO とある箇所)


== 起動時の CPU レジスタ

出典: mooneye `misc/boot_regs-cgb` (実機の CGB で検証済み)。

[cols="1,1,1"]
|===
| レジスタ | DMG | CGB (DMG 互換モード)

| A | 0x01 | 0x11 (CGB ハードの識別値)
| F | 0xB0 | 0x80 (Z=1)
| B | 0x00 | ライセンス依存 (下記)
| C | 0x13 | 0x00
| D | 0x00 | 0x00
| E | 0xD8 | 0x08
| H | 0x01 | 0x00
| L | 0x4D | 0x7C
| SP | 0xFFFE | 0xFFFE
| PC | 0x0100 | 0x0100
|===

=== B レジスタ (CGB)

Pan Docs "Power-Up Sequence" 脚注 3: 旧ライセンスコードが `0x01`、または `0x33` かつ
新ライセンスコードが `"01"` (= 任天堂) なら、タイトル 16 バイト (`0x0134-0x0143`) の総和。
それ以外は `0x00`。DMG では常に `0x00`。
実装は `Memory.cgbBootB`、ROM ロード時 (`Decoder.loadRomToState`) に B へ反映する。

== 起動時の I/O レジスタ (CGB)

出典: mooneye `misc/boot_hwio-C` (実機の CGB で検証済み)。
同 ROM は `0xFF04` (DIV)・`0xFF30-0xFF3F` (波形 RAM)・`0xFF40` (LCDC)・`0xFF41` (STAT)・`0xFF44` (LY) を
比較対象から外している (タイミング依存のため)。

DMG と値が異なるのは次のレジスタのみ:

[cols="1,1,1,3"]
|===
| アドレス | 名称 | CGB の値 | 備考

| 0xFF00 | P1 | 0xFF | CGB のブート ROM が 0x30 を書く (どちらのボタン群も非選択)
| 0xFF04 | DIV | 0x26 | 内部カウンタ 0x2681 の上位バイト (下記)
| 0xFF48/0xFF49 | OBP0/OBP1 | 0x00 | DMG では未初期化で 0xFF
| 0xFF68 | BCPS | 0xC8 | 互換モードの残響値
| 0xFF6A | OCPS | 0xD0 | 同
| 0xFF72/0xFF73 | 未定義 | 0x00 |
| 0xFF75 | 未定義 | 0x8F |
| 0xFF76/0xFF77 | 未定義 | 0x00 |
|===

それ以外 (TIMA/TMA/TAC/IF/APU/波形 RAM/LCDC/STAT/SCY/SCX/LY/LYC/DMA/BGP/WY/WX) は DMG と同じ値。
CGB 専用レジスタは DMG 互換モードなので `0xFF`。

== DIV の初期値 (内部カウンタ 0x2681)

DMG と同じ手順で、mooneye `misc/boot_div-cgbABCDE` の期待値
(`27, 28, 28, 29, 2A, 2C`) から逆算する。

同 ROM は `nop` / `jp 0x0150` の後、nop 数だけを変えて `ldh a,(DIV)` を 6 回読む。
CGB 版は最初の nops が 27 なので、読み出し開始時点の累積 T サイクルは
128 / 384 / 636 / 892 / 1148 / 1408。両立するリセット時カウンタは 4 値の窓 `0x2680..0x2683` に絞られ、
「読み出しは DIV 更新直後」という意図 (`0x2701 - 128`) から **0x2681** を採る。

注意: CGB のブート ROM の長さは実際にはヘッダ内容と互換パレット選択に依存する (Pan Docs 脚注 5)。
この固定値は mooneye のテスト ROM に合わせた近似であり、任意の CGB カートで実機と一致する保証はない。

== 検証状況 (実測)

[cols="2,1,3"]
|===
| テスト ROM | 結果 | 備考

| misc/boot_regs-cgb | PASS | CPU レジスタ初期値 (A=0x11 等)
| misc/boot_div-cgbABCDE | PASS | DIV の値と位相
| misc/boot_hwio-C | FAIL | `0xFF26` NR52: expected 0xF1 / got 0xF0
| misc/bits/unused_hwio-C | FAIL | 未使用ビットの読み値テーブル (未調査)
| misc/boot_div-cgb0 | FAIL | CGB0 ブート ROM 固有の値 (対象外)
|===

`boot_hwio-C` の NR52 bit0 は「ブート ROM の起動ジングルがまだ鳴っている」状態で、
DMG 版 (`acceptance/boot_hwio-dmgABCmgb`) と同じくブート ROM なしでは再現できない。
それ以外の全レジスタは一致している。

=== unit test

* `MachineModeTests` がマシン種別・CGB モード解禁・バンク・KEY1/STOP・倍速のクロック配分を直接検証する
* 倍速の周辺クロック配分と STOP の停止動作は**検証 ROM が無い**ため unit test のみ
  (Pan Docs の記述に基づく実装であり、実機での確認は未実施)
* DMG の回帰は公開 ROM 161 本で確認する (`docs/power-on.md` と同じ手順)

== 実装中に見つけた既存のバグ

* **電源投入直後の OAM DMA 誤爆**: `Ppu.step` が `0xFF46` のレジスタ値で DMA を判定していたため、
  起動値 `0xFF` を「DMA 要求」と解釈して 160 バイトを OAM へ転送し、PPU のサイクルを
  160 減らしていた (PPU のモード/LY タイミングが起動直後からずれる)。
  `MemoryBus.DmaRequest` を追加し、`0xFF46` への**書き込み**でのみ転送するようにした
* **倍速判定の誤り**: 非 CGB モードでは `0xFF4D` が `0xFF` を読むため、Io の生値の bit7 を
  見ると DMG でも倍速と判定され、PPU/APU が半分の速度で動いていた。
  `CgbMode` でゲートして修正 (unit test `DMG では 0xFF4D が 0xFF でも倍速と誤判定しない` で固定)

== 実 ROM を走らせる (ヘッドレス検証)

実カートのダンプを動かして画面を目視で確認する経路:

[source,sh]
----
nix develop -c dotnet fsi tools/dump_frames.fsx -- <rom.gb> [--machine cgb] [--frames 120] [--out frame.png]
----

* `Emulator.createWithMachine Memory.Cgb` で CGB マシンとして起動する
  (カート側の `0x0143` bit7 で CGB モードが解禁される)。`Emulator.reset` もマシン種別を維持する
* バッテリー付きカートは ROM の隣の `.sav` を読む (書き戻しはしない)
* 出力は 8bit グレースケール (DMG の 4 階調を 255/170/85/0 に対応)。
  CGB のカラー描画を実装したら RGB 出力に切り替える
* 実 ROM は著作物なので**リポジトリには入れない** (ローカルの検証素材として扱う)

== 未実装 (今後の課題)

* **CGB パレット** (BCPS/BCPD = 0xFF68/0xFF69, OCPS/OCPD = 0xFF6A/0xFF6B) とカラー描画。
  現状の PPU は DMG の 4 階調のみで、VRAM バンク 1 の属性 (パレット番号/反転/優先度) も読まない。
  **実 CGB カートを動かすにはここが必要** (次のスライス)
* **HDMA (0xFF51-0xFF55)**、**赤外線 (RP = 0xFF56)**
* 倍速時のシリアル / OAM DMA の速度 (現状は CPU とタイマのみ 2 倍)
* `Emulator` (Desktop / MCP) は常に DMG で起動する。CGB カートを動かすには
  マシン種別を選ぶ経路 (UI / MCP ツール) が必要
