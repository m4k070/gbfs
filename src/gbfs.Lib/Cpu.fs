namespace gbfs.Lib

module Cpu =
  type Registers =
    | A of int8
    | B of int8
    | C of int8
    | D of int8
    | E of int8
    | F of int8
    | PC of int16

  let nop registers memory = registers memory 
