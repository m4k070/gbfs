# カートリッジ対応 (MBC / バッテリー / RTC)

`Memory.fs` が実装するカートリッジ側の仕様と、その検証状況をまとめる。
外部仕様の出典は Pan Docs (gbdev.io/pandocs) の MBC1/MBC2/MBC3/MBC5 各節。

## 対応マッパー

ROM ヘッダ `0x0147` で判定する (`Memory.detectMbcType`)。

| 0x0147 | マッパー | ROM bank | RAM | 備考 |
|---|---|---|---|---|
| 0x00, 0x08, 0x09 | ROM のみ | 固定 (0x4000 は bank 1) | RAM は常時有効 | 0x09 はバッテリー付き |
| 0x01-0x03 | MBC1 | 5bit + 上位 2bit | 2/8/32 KiB | 0x03 はバッテリー付き |
| 0x05, 0x06 | MBC2 | 4bit | 内蔵 512 nibble | 0x06 はバッテリー付き |
| 0x0F-0x13 | MBC3 | 7bit | 8/32 KiB + RTC | 0x0F/0x10/0x13 はバッテリー付き |
| 0x19-0x1E | MBC5 | 9bit | 8/32/128 KiB | ランブルは未実装 (RAM bank として扱う) |

未対応: MMM01 / MBC6 / MBC7 / HuC1 / HuC3 / MBC30、CGB 専用機能。

## 実装上の注意 (取り違えやすい点)

- **MBC1 の 00→01 変換は「読み出し時」に 5bit 全域で判定する**。`RomBank` には生の 5bit 値を保持し、
  変換とバンク数マスクは `maskBank` で行う。256 KiB 以下のカートでは 5bit 目を立てると
  bank 0 を 0x4000-7FFF に置ける (Pan Docs の記述どおり)。
- **MBC1 mode 0 は RAM bank を 0 に固定する**。mode 1 のみ 2bit レジスタが RAM bank を選ぶ。
- **MBC1 mode 1 の 0000-3FFF は 2bit レジスタで丸ごと差し替わる** (banks $20/$40/$60)。
- **MBC1M (マルチカート) は別配線**。2bit レジスタが ROM bank の bit4-5 に入り、5bit レジスタの bit4 は無視される。
  判定は「bank $10 の先頭にも Nintendo ロゴ (0x0104-0x0133) があるか」。
- **MBC2 はアドレス bit8 でレジスタを選ぶ** (0=RAM enable / 1=ROM bank)。0x0000-0x3FFF 全域でこの規則。
  RAM は下位 9bit で索引し、上位 4bit は不定 (0xF を返す)。RAM サイズヘッダは 0x00 のままなので
  `getRamSize` が MBC2 だけ 512 byte を返す。
- **MBC5 は bank 0 が有効** (00→01 変換なし)。0x2000 に下位 8bit、0x3000 に bit8。
- **RAM アクセスは常にカートの RAM サイズでマスクする**。8 KiB RAM のカートで RAM bank を変えても
  同じ RAM が見える (実機の配線と同じ)。マスクを忘れると 0xFF が返って mooneye の `mbc1/ram_64kb` が落ちる。
- **MBC3 の RTC は実時間ベース** (`BaseSeconds` + 経過 unix 秒)。実機同様、電源断中も進むため `.sav` には
  保存しない。読み出しは 0x6000-0x7FFF への `$00`→`$01` ラッチ時のスナップショットのみを返す。
  日カウンタは 9bit で、512 日で桁溢れビットが立つ。`Memory.setRtcClock` でテスト時に時刻を固定できる。

## バッテリーバックアップ (.sav)

- 保存先は ROM と同名で拡張子だけ `.sav` (`Emulator.saveRamPath`)。
- `Emulator.loadRomWithSave` が ROM 読込時に復元、`Emulator.saveRam` が書き出す
  (バッテリーフラグが無いカートでは何もしない)。
- Desktop UI: 起動時 (ROM 引数) に復元、Stop ボタンとウィンドウ終了時に保存。ステータス行に
  `MBC3+BATT` のように表示。
- MCP: `load_rom` が復元、`save_ram` ツールが明示保存。
- `.sav` の長さは ROM ヘッダの RAM サイズで決まる。大きいファイルは先頭のみ採用、小さい場合は 0 埋め。

## 検証状況

| 対象 | 手段 | 結果 (2026-10-02) |
|---|---|---|
| MBC1 / MBC2 / MBC5 | mooneye `emulator-only/mbc{1,2,5}` | 28/28 PASS |
| MBC3 バンク切り替え / RTC / .sav | `tests/gbfs.Tests/MbcTests.fs` (34 件) | 全 PASS |

mooneye-test-suite に MBC3 のテストは無いため、MBC3 と RTC は unit test が唯一の検証手段。
実カートリッジ ROM での動作は未検証 (手元に ROM が無い)。
