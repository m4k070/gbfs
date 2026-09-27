module TestRomVerdictTests

open Xunit
open TestRomVerdict
open gbfs.Lib.Decoder

// tools/TestRomVerdict.fs はテスト ROM ランナー (tools/run_test_roms.fsx) と
// wwc 側ハーネスで共有する判定関数。ここで判定の契約を固定する。

module BlarggTextTests =
    [<Fact>]
    let ``出力に終了語がなければ実行中`` () =
        Assert.Equal(InProgress, judgeBlarggText "01-special\n\n\n")

    [<Fact>]
    let ``Passed で成功`` () =
        Assert.Equal(Concluded Passed, judgeBlarggText "01-special\n\n\nPassed\n")

    [<Fact>]
    let ``Failed で失敗し、出力全文を理由に残す`` () =
        let text = "instr_timing\n\n04:3-1 \nFailed\n"
        Assert.Equal(Concluded (Failed (text.Trim())), judgeBlarggText text)

    [<Fact>]
    let ``合成版の Passed all tests も成功`` () =
        Assert.Equal(Concluded Passed, judgeBlarggText "01:ok  02:ok  \n\nPassed all tests")

    [<Fact>]
    let ``シリアル 1 バイトを文字として積む`` () =
        Assert.Equal("Pa", appendSerialByte "P" (byte 'a'))

module BlarggCartRamTests =
    let ram status = { Status = status; Signature = BlarggCartRamSignature; Text = "halt bug\n" }

    [<Fact>]
    let ``署名がなければ実行中扱い`` () =
        Assert.Equal(InProgress, judgeBlarggCartRam { ram 0uy with Signature = [| 0uy; 0uy; 0uy |] })

    [<Fact>]
    let ``状態 0x80 は実行中`` () =
        Assert.Equal(InProgress, judgeBlarggCartRam (ram BlarggStatusRunning))

    [<Fact>]
    let ``状態 0 は成功`` () =
        Assert.Equal(Concluded Passed, judgeBlarggCartRam (ram 0uy))

    [<Fact>]
    let ``状態 0 以外は失敗`` () =
        match judgeBlarggCartRam (ram 2uy) with
        | Concluded (Failed reason) -> Assert.StartsWith("result code 2", reason)
        | other -> Assert.Fail(sprintf "expected Failed, got %A" other)

    [<Fact>]
    let ``シリアルで決まればカートリッジ RAM より優先`` () =
        Assert.Equal(Concluded Passed, judgeBlargg "Passed" (Some (ram 1uy)))

    [<Fact>]
    let ``シリアルが未確定ならカートリッジ RAM で判定`` () =
        Assert.Equal(Concluded Passed, judgeBlargg "" (Some (ram 0uy)))

module MooneyeTests =
    let failRegs = { B = 0x42uy; C = 0x42uy; D = 0x42uy; E = 0x42uy; H = 0x42uy; L = 0x42uy }

    [<Fact>]
    let ``フィボナッチ数なら成功`` () =
        Assert.Equal(Passed, judgeMooneye MooneyePassRegisters None)

    [<Fact>]
    let ``全レジスタ 0x42 なら失敗`` () =
        match judgeMooneye failRegs None with
        | Failed reason -> Assert.StartsWith("fail marker", reason)
        | other -> Assert.Fail(sprintf "expected Failed, got %A" other)

    [<Fact>]
    let ``assert の差分を失敗理由に含める`` () =
        // if_ie_registers: C と E を検査 (flags bit3, bit5)、IF の上位ビットが 1 で読めない場合
        let saved = [| 0uy; 0uy; 0x08uy; 0uy; 0x00uy; 0uy; 0uy; 0uy |]
        let expected = [| 0uy; 0uy; 0xE8uy; 0uy; 0xE0uy; 0uy; 0uy; 0uy |]
        let assertions = { Saved = saved; Flags = 0b0010_1000uy; Expected = expected }
        Assert.Equal<string list>([ "C=08 (expected E8)"; "E=00 (expected E0)" ], mooneyeAssertionMismatches assertions)

    [<Fact>]
    let ``検査しないレジスタの差分は無視`` () =
        let assertions = { Saved = Array.create 8 1uy; Flags = 0uy; Expected = Array.create 8 2uy }
        Assert.Empty(mooneyeAssertionMismatches assertions)

module FinalizeTests =
    [<Fact>]
    let ``上限到達で未確定なら Timeout (FAIL と区別)`` () =
        Assert.Equal(Timeout "limit", finalizeAtLimit "limit" InProgress)

    [<Fact>]
    let ``上限到達でも確定済みならその判定`` () =
        Assert.Equal(Passed, finalizeAtLimit "limit" (Concluded Passed))

    [<Fact>]
    let ``mts- ディレクトリは mooneye とみなす`` () =
        Assert.Equal(MooneyeFibonacci, inferProtocol "/roms/mts-20260714/acceptance/ei_timing.gb")
        Assert.Equal(BlarggSerial, inferProtocol "/roms/blargg/cpu_instrs/individual/01-special.gb")

module StepWithCyclesTests =
    [<Fact>]
    let ``NOP は 4 サイクル`` () =
        let state = loadRomToState [| 0x00uy |] (createState ())
        let state = { state with Regs = { state.Regs with PC = 0us } }
        let (_, cycles) = stepWithCycles state
        Assert.Equal(4, cycles)

    [<Fact>]
    let ``step は stepWithCycles の状態部分と一致`` () =
        // LD A,$42 (8 サイクル)
        let state = loadRomToState [| 0x3Euy; 0x42uy |] (createState ())
        let state = { state with Regs = { state.Regs with PC = 0us } }
        let (withCycles, cycles) = stepWithCycles state
        let plain = step state
        Assert.Equal(8, cycles)
        Assert.Equal(withCycles.Regs, plain.Regs)
