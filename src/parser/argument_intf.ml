open! Import

exception Unwrap_failed

type ('reg, 'rel) t =
  | Reg of 'reg
  | Rel of 'rel Relocatable.t
  | Base_offset of 'rel Relocatable.t * 'reg
  | StringLiteral of string
  | Uninit
[@@deriving eq, show]
