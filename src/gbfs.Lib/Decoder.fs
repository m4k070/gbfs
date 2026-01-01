namespace gbfs.Lib

open Cpu

module Decoder =
  // オペコードのビット列(0-7)に対応するレジスタの定義
  // 例: 000->B, 001->C, ... 111->A
  let private r8map =
    [|
      R8 Cpu.B;
      R8 Cpu.C;
      R8 Cpu.D;
      R8 Cpu.E;
      R8 Cpu.H;
      R8 Cpu.L;
      R16 Cpu.HL;
      R16 Cpu.AF;
    |] 
                                                                                 
  // NOP命令 (0x00)
  let (|Nop|_|) (opcode: uint8) =
    if opcode = 0x00uy then Some() else None
                                                                                 
  // 8bitロード命令: LD r, r' (0x40 - 0x7F, ただし 0x76(HALT)を除く)
  // パターン: 01xxxyyy (xxx=dst, yyy=src)
  let (|LdRR|_|) (opcode: uint8) =
    if opcode >= 0x40uy && opcode <= 0x7Fuy && opcode <> 0x76uy then
      let dst_idx = (int opcode >>> 3) &&& 0x7
      let src_idx = int opcode &&& 0x7
      Some(r8map.[dst_idx], r8map.[src_idx])
    else
      None
                                                                                 
  // 8bit加算命令: ADD A, r (0x80 - 0x87)
  // パターン: 10000xxx (xxx=src)
  let (|AddR|_|) (opcode: uint8) =
    if opcode >= 0x80uy && opcode <= 0x87uy then
      let src_idx = int opcode &&& 0x7
      Some(r8map.[src_idx])
    else
      None

  let rec Decode (memory: byte list) (reg: Cpu.Register)  =
    let _, b = List.splitAt (int(reg.PC)) memory
    let advancePc n reg = { reg with PC = reg.PC + uint16 n }

    match b with
    | [] -> reg 
    | opcode :: rest -> 
      let nextReg =
        match opcode with
        | Nop ->
          reg |> Cpu.Nop |> advancePc 1
        | LdRR (R8 dst, R8 src) ->
          reg |> Cpu.LoadR dst src |> advancePc 1
        | AddR (R8 src) ->
          reg |> Cpu.AddR src |> advancePc 1
        | _ ->
          reg |> advancePc 1
      Decode memory nextReg 

