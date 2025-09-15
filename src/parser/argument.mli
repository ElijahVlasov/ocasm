open! Import

exception Unwrap_failed

type ('reg, 'rel) t =
  | Reg of 'reg
  | Rel of 'rel Relocatable.t
  | Base_offset of 'rel Relocatable.t * 'reg
  | StringLiteral of string
  | Uninit
[@@deriving eq, show]

val unwrap_reg_thunk : default:(unit -> 'reg) -> ('reg, _) t -> 'reg

val unwrap_rel_thunk :
  default:(unit -> 'rel Relocatable.t) -> (_, 'rel) t -> 'rel Relocatable.t

val unwrap_base_offset_thunk :
  default:(unit -> 'rel Relocatable.t * 'reg) ->
  ('reg, 'rel) t ->
  'rel Relocatable.t * 'reg

val unwrap_string_literal_thunk : default:(unit -> string) -> (_, _) t -> string
val unwrap_reg_exn : ('reg, _) t -> 'reg
val unwrap_rel_exn : (_, 'rel) t -> 'rel Relocatable.t
val unwrap_base_offset_exn : ('reg, 'rel) t -> 'rel Relocatable.t * 'reg
val unwrap_string_literal_exn : (_, _) t -> string
