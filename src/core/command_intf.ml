open! Import

type ('instr, 'dir) t =
  | Instruction of 'instr
  | Directive of 'dir
  | Label of string
  | Eof
[@@deriving eq, show]
