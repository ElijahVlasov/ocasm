open! Import

type t =
  | Initial
  | Label
  | Mb_opcode
  | Mb_directive
  | Operand of Isa.Type.t list
