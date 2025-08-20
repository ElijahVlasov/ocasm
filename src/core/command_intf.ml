open! Import

type ('instr, 'dir) t =
  | Instruction of 'instr
  | Directive of 'dir
  | Label of string
[@@deriving eq, show]
