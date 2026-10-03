= 電源投入直後のハードウェア状態 (Power-On State)
:toc:

gbfs はブート ROM を実行せず、`PC=0x0100` から直接ゲーム ROM を実行する。
そのため「ブート ROM が完了した時点」のハードウェア状態を `Decoder.createState ()` で再現している。
本ドキュメントはその根拠と、検証状況・未達項目をまとめる。

== 実装場所

* `src/gbfs.Lib/Memory.fs` — `postBootIo` (I/O レジスタ 0xFF00-0xFF7F の初期値) と
  NR14/NR24/NR34/NR44 の読み出し時 bit7 マスク
* `src/gbfs.Lib/Timer.fs` — `postBootCounter` (内部 16bit カウンタの初期値)
* `src/gbfs.Lib/Cpu.fs` — `initRegister` (A/F/B/C/D/E/H/L/SP/PC)
* `src/gbfs.Lib/Apu.fs` — トリガ要求 (NRx4 bit7) は Io 配列の生値を直接参照する

== レジスタ初期値 (DMG / MGB)

出典: Pan Docs "Power-Up Sequence" の DMG/MGB 列。
同表は mooneye-test-suite `acceptance/boot_regs-dmgABC` / `boot_hwio-dmgABCmgb` (実機検証済み) を根拠とする。

[cols="1,1,1,3"]
|===
| アドレス | 名称 | 値 | 備考

| 0xFF00 | P1 | 0xCF | ボタン選択なし・押下なし
| 0xFF01 | SB | 0x00 |
| 0xFF02 | SC | 0x7E |
| 0xFF04 | DIV | 0xAB | 内部カウンタの上位バイト (下記参照)
| 0xFF05 | TIMA | 0x00 |
| 0xFF06 | TMA | 0x00 |
| 0xFF07 | TAC | 0xF8 | タイマ停止 (bit2=0), 4096Hz 選択
| 0xFF0F | IF | 0xE1 | VBlank フラグ + 未使用ビットは 1
| 0xFF10-0xFF26 | APU (NR10-NR52) | 表参照 | Pan Docs の表と同値
| 0xFF30-0xFF3F | 波形 RAM | 0x00 | ブート直後の値は未検証 (mooneye も比較対象外)
| 0xFF40 | LCDC | 0x91 | LCD on / BG on / OBJ on
| 0xFF41 | STAT | 0x85 | VBlank モード + LYC=LY
| 0xFF42/0xFF43 | SCY / SCX | 0x00 |
| 0xFF44 | LY | 0x00 |
| 0xFF45 | LYC | 0x00 |
| 0xFF46 | DMA | 0xFF |
| 0xFF47 | BGP | 0xFC |
| 0xFF48/0xFF49 | OBP0 / OBP1 | 0xFF | 実機では未初期化 (不定)
| 0xFF4A/0xFF4B | WY / WX | 0x00 |
| 0xFF4C-0xFF7F | CGB 専用・未使用 | 0xFF | 非 CGB モードでは 0xFF を読む
| 0xFFFF | IE | 0x00 |
|===

CPU レジスタ (`boot_regs-dmgABC` が要求する値):
A=0x01, F=0xB0, B=0x00, C=0x13, D=0x00, E=0xD8, H=0x01, L=0x4D, SP=0xFFFE, PC=0x0100。

== DIV の初期値 (内部カウンタ 0xABD5)

`DIV = 内部カウンタ >> 8` なので、Pan Docs の `DIV=0xAB` は上位バイトのみを決める。
低位バイトは `acceptance/boot_div-dmgABCmgb` が要求する位相から逆算した。

同 ROM は PC=0x0100 で `nop` / `jp 0x0150` を実行してから、`nop` 数だけを変えながら
`ldh a,(DIV)` を 6 回読み、`AC, AD, AD, AE, AF, B1` を要求する
(読み出し時点で DIV が更新された直後か直前期前かで値が変わることを検証している)。

各 `ldh a,(DIV)` 開始時点までの累積 T サイクルは 44 / 300 / 552 / 808 / 1064 / 1324
(6 nops, 57 nops, 56 nops, 57 nops, 57 nops, 58 nops + push/pop)。
gbfs は命令実行後にタイマを進めるため、読み出しは命令開始時点のカウンタを見る。
6 つの期待値すべてと両立するリセット時カウンタは `0xABD4..0xABD7` の 4 値に絞られ、
テストの「読み出しは DIV 更新直後」という意図 (`0xAC01 - 44`) に一致する **0xABD5** を採る。

注意: `Memory.postBootIo` の DIV (0xFF04) と `Timer.postBootCounter` の上位バイトは一致させること。
`Timer.step` は「Io の DIV とカウンタの不一致」を DIV 書き込み (リセット) と解釈するため、
食い違っていると最初の step でカウンタが 0 に戻る。

== NR14/NR24/NR34/NR44 の bit7 (トリガ)

* 実機: bit7 は書き込み専用。読み出しは常に 1。
* gbfs: Io 配列の bit7 を「トリガ要求 (pending)」フラグとして使い、`Apu.step` が要求を消費して 0 に戻す。
  そのため読み出し経路 (`Memory.read`) で bit7 を 1 にマスクし、
  `Apu.readChannelRegisters` は Io 配列の生値を直接読む。
* 初期値は 0x3F (要求なし)。0xBF にすると起動直後に APU が全チャンネルを誤トリガーする。

== 検証状況

[cols="2,1,3"]
|===
| テスト ROM | 結果 | 備考

| acceptance/boot_regs-dmgABC | PASS | CPU レジスタ初期値
| acceptance/boot_div-dmgABCmgb | PASS | DIV の値と位相 (実装前は FAIL)
| acceptance/div_timing | PASS | DIV 書き込みと位相 (実装前は FAIL)
| acceptance/timer/tim01 | PASS | TIMA レート bit3 (実装前は FAIL)
| acceptance/boot_hwio-dmgABCmgb | FAIL | 下記の残差
| acceptance/serial/boot_sclk_align-dmgABCmgb | FAIL | シリアルのクロック分周が未実装
|===

=== 公開 ROM スイート全体での効果 (161 ROM, 実測)

* 実装前 (commit 0541757 / 70ad131): PASS 64 / 161
* 実装後: **PASS 71 / 161 (回帰 0)**
* 新規 PASS (7 本): `boot_div-dmgABCmgb`, `div_timing`, `timer/tim01`, `timer/rapid_toggle`,
  `intr_timing`, `halt_ime0_nointr_timing`, `halt_ime1_timing2-GS`
* 詳細が改善 (FAIL のまま): `dmg_sound/04-sweep` (失敗コード 4 → 3),
  `pop_timing` (不一致 2 → 1)
* `boot_div-S` / `boot_div-dmg0` / `boot_div2-S` は SGB / DMG0 の期待値 (D9 / 19) を要求するため
  DMG エミュレータでは FAIL が正しい (現在は DMG の値 AC/AD を返している)

=== boot_hwio-dmgABCmgb の残差 (実測)

実測した最初の不一致 (mismatch_addr/expected/got を ROM の .sym から読む):

1. 0xFF14 NR14: expected 0xBF / got 0x3F — APU がトリガビットをクリアしていた → **修正済み**
2. 0xFF26 NR52: expected 0xF1 / got 0xF0 — bit0 (チャンネル 1 動作中) はブート ROM の
   起動ジングルの残響。SGB では 0xF0 であることからも、ブート ROM 依存の状態と確認できる。
   ブート ROM を実行しない限り、あるいは残響状態を意図的に再現しない限り一致しない → 未対応
3. 0xFF41 STAT: expected 0x80 (モード 0 = HBlank) — PPU のモード遷移と厳密な命令サイクルに依存 → 未対応
4. 0xFF44 LY: expected 0x0A — テストのループ総サイクル数に依存 → 未対応

2〜4 はブート ROM か、命令/PPU のタイミング精度 (課題B) が必要。
本ドキュメントの範囲は「ブート ROM なしで決まる静的な初期値」であり、そこは一致済み。

== 未対応 (今後の課題)

* CGB モード (`KEY0`/`KEY1`/パレット/VBK)。非 CGB モードでは 0xFF4C-0xFF7F が 0xFF を読むため、
  現状の 0xFF 埋めは「DMG として正しい」値である。
* ブート ROM の実行 (Pan Docs は 9 種の公式ブート ROM の存在を記載。ライセンス上、同梱は不可)。
