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

== 未実装 (今後の課題)

* **KEY0 (0xFF4C) / KEY1 (0xFF4D) と STOP による倍速切り替え**。
  倍速時は CPU・タイマ・シリアル・OAM DMA が 2 倍、LCD と APU は等速 (Pan Docs)。
  STOP 後の CPU 停止は 2050 M サイクル。手元の mooneye/blargg にこれを検証する ROM が無いため、
  実装する場合は unit test のみで検証し「未検証」と明示する。
* **CGB パレット** (BCPS/BCPD, OCPS/OCPD) とカラー描画。現状の PPU は DMG の 4 階調のみ。
* **VRAM バンク (VBK) / WRAM バンク (SVBK)**。現状は書き込みを無視する (レジスタの初期値のみ正しい)。
* **HDMA (0xFF51-0xFF55)**、**赤外線 (RP)**、**OPRI**。
* `Emulator` (Desktop / MCP) は常に DMG で起動する。CGB カートを動かすには
  マシン種別を選ぶ経路 (UI / MCP ツール) が必要。
