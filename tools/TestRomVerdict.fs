/// 公開テスト ROM (blargg / mooneye-test-suite) の合否判定。
///
/// エミュレータ本体 (gbfs.Lib) に依存しない純粋関数だけを置く。
/// gbfs のランナー (tools/run_test_roms.fsx) と、将来の wwc 側ハーネス
/// (RTL ゲートレベルシミュレーション) の両方から `#load` して同じ判定を使うため。
///
/// ハーネス側の責務は「観測」だけ:
///   - シリアル: 0xFF02 に 0x81 が書かれたときの 0xFF01 の値を `appendSerialByte` で積む
///   - blargg の結果メモリ: 0xA000.. を `BlarggCartRam` に詰める
///   - mooneye: `LD B,B` (0x40) を実行する直前のレジスタを `MooneyeRegisters` に詰める
/// 判定 (いつ終わったか・成否) はこのモジュールが決める。
module TestRomVerdict

// ============================================================
// 判定結果
// ============================================================

/// 1 本の ROM の最終判定
type Verdict =
    | Passed
    | Failed of reason: string
    /// 命令数・サイクル数などの上限に達した (FAIL とは区別する)
    | Timeout of limit: string

/// 実行途中の判定。`Concluded` になった時点でハーネスは実行を止めてよい
type Progress =
    | InProgress
    | Concluded of Verdict

/// 合否の通知方式
type Protocol =
    /// blargg: シリアルに文字を出し、最後に "Passed" / "Failed" を出す。
    /// 一部の版は 0xA000 (カートリッジ RAM) にも結果コードと文字列を書く
    | BlarggSerial
    /// mooneye-test-suite: 終了時に `LD B,B` を実行し、B/C/D/E/H/L で成否を示す
    | MooneyeFibonacci

// ============================================================
// blargg: シリアル出力
// ============================================================

/// SC (0xFF02) にこの値が書かれたら内部クロックでの転送開始 (blargg はこれで 1 文字送る)
[<Literal>]
let SerialStartInternalClock = 0x81uy

/// シリアルに 1 バイト送られたときの出力ログ更新 (純粋)
let appendSerialByte (log: string) (value: byte) : string =
    log + string (char value)

/// 出力テキストから blargg の終了語 ("Passed" / "Failed") を探す。
/// cpu_instrs 個別版は成功時 "Passed"、失敗時は失敗 opcode の一覧に続けて "Failed"。
/// 合成版 (cpu_instrs.gb) は "Passed all tests" / "Failed N tests"。
let judgeBlarggText (text: string) : Progress =
    let passedAt = text.IndexOf("Passed", System.StringComparison.Ordinal)
    let failedAt = text.IndexOf("Failed", System.StringComparison.Ordinal)
    match passedAt, failedAt with
    | -1, -1 -> InProgress
    | _, f when f >= 0 -> Concluded (Failed (text.Trim()))
    | _ -> Concluded Passed

// ============================================================
// blargg: カートリッジ RAM (0xA000) の結果
// ============================================================

/// 0xA001-0xA003 にこの 3 バイトがあれば結果メモリが有効
let BlarggCartRamSignature = [| 0xDEuy; 0xB0uy; 0x61uy |]

/// 0xA000 がこの値の間はテスト実行中
[<Literal>]
let BlarggStatusRunning = 0x80uy

/// 0xA000 から読んだ blargg の結果領域
type BlarggCartRam = {
    /// 0xA000: 0x80 = 実行中、0x00 = 成功、それ以外 = 失敗コード
    Status: byte
    /// 0xA001-0xA003
    Signature: byte array
    /// 0xA004 からの 0 終端文字列
    Text: string
}

let judgeBlarggCartRam (ram: BlarggCartRam) : Progress =
    let hasSignature = ram.Signature = BlarggCartRamSignature
    if not hasSignature then InProgress
    elif ram.Status = BlarggStatusRunning then InProgress
    elif ram.Status = 0uy then Concluded Passed
    else Concluded (Failed (sprintf "result code %d: %s" ram.Status (ram.Text.Trim())))

/// シリアルを優先し、シリアルで決まらなければカートリッジ RAM を見る
let judgeBlargg (serialText: string) (cartRam: BlarggCartRam option) : Progress =
    match judgeBlarggText serialText, cartRam with
    | Concluded v, _ -> Concluded v
    | InProgress, Some ram -> judgeBlarggCartRam ram
    | InProgress, None -> InProgress

// ============================================================
// mooneye: LD B,B 時点のレジスタ
// ============================================================

/// mooneye の終了マーカー `LD B,B`
[<Literal>]
let MooneyeBreakpointOpcode = 0x40uy

type MooneyeRegisters = { B: byte; C: byte; D: byte; E: byte; H: byte; L: byte }

/// 成功時の B/C/D/E/H/L (フィボナッチ数)
let MooneyePassRegisters = { B = 3uy; C = 5uy; D = 8uy; E = 13uy; H = 21uy; L = 34uy }

/// 失敗時は全レジスタが 0x42
[<Literal>]
let MooneyeFailMarker = 0x42uy

let formatMooneyeRegisters (r: MooneyeRegisters) : string =
    sprintf "B=%02X C=%02X D=%02X E=%02X H=%02X L=%02X" r.B r.C r.D r.E r.H r.L

/// mooneye の assert マクロ (common/lib/check_asserts_cb.s) が HRAM に残す値。
/// ROM と同名の .sym にある hram.regs_save / hram.regs_flags / hram.regs_assert から読む。
/// assert を使わない ROM (タイミング系の多く) には無い
type MooneyeAssertions = {
    /// hram.regs_save: 実測値。並びは F, A, C, B, E, D, L, H
    Saved: byte array
    /// hram.regs_flags: bit ごとに「このレジスタを検査する」
    Flags: byte
    /// hram.regs_assert: 期待値。並びは Saved と同じ
    Expected: byte array
}

/// regs_flags の bit 番号 → (レジスタ名, regs_save / regs_assert 内のオフセット)
let private mooneyeAssertionSlots =
    [ 0, "A", 1; 1, "F", 0; 2, "B", 3; 3, "C", 2
      4, "D", 5; 5, "E", 4; 6, "H", 7; 7, "L", 6 ]

/// 検査対象のうち期待値と違ったレジスタを "C=E8 (expected E0)" の形で列挙する
let mooneyeAssertionMismatches (a: MooneyeAssertions) : string list =
    [ for (bit, name, offset) in mooneyeAssertionSlots do
        let isChecked = (a.Flags >>> bit) &&& 1uy = 1uy
        let actual = a.Saved.[offset]
        let expected = a.Expected.[offset]
        if isChecked && actual <> expected then
            yield sprintf "%s=%02X (expected %02X)" name actual expected ]

/// `LD B,B` を実行した時点で判定は必ず確定する
let judgeMooneye (regs: MooneyeRegisters) (assertions: MooneyeAssertions option) : Verdict =
    let isFailMarker =
        [ regs.B; regs.C; regs.D; regs.E; regs.H; regs.L ]
        |> List.forall (fun v -> v = MooneyeFailMarker)
    let assertionDetail =
        match assertions |> Option.map mooneyeAssertionMismatches with
        | Some [] | None -> ""
        | Some mismatches -> "; assert mismatches: " + String.concat ", " mismatches
    if regs = MooneyePassRegisters then Passed
    elif isFailMarker then Failed ("fail marker (0x42)" + assertionDetail)
    else Failed ("unexpected registers at LD B,B: " + formatMooneyeRegisters regs + assertionDetail)

// ============================================================
// 終了処理
// ============================================================

/// 上限に達した時点の進捗を最終判定に変換する (未確定なら Timeout)
let finalizeAtLimit (limitDescription: string) (progress: Progress) : Verdict =
    match progress with
    | Concluded v -> v
    | InProgress -> Timeout limitDescription

/// パスから通知方式を推定する (mooneye のビルド済み ZIP は "mts-" ディレクトリに展開される)
let inferProtocol (romPath: string) : Protocol =
    let p = romPath.Replace('\\', '/').ToLowerInvariant()
    let isMooneye = p.Contains("/mts-") || p.Contains("mooneye")
    if isMooneye then MooneyeFibonacci else BlarggSerial

let verdictLabel (v: Verdict) : string =
    match v with
    | Passed -> "PASS"
    | Failed _ -> "FAIL"
    | Timeout _ -> "TIMEOUT"
