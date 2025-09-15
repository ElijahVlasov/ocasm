open! Import

exception Unwrap_failed

type ('reg, 'rel) t =
  | Reg of 'reg
  | Rel of 'rel Relocatable.t
  | Base_offset of 'rel Relocatable.t * 'reg
  | StringLiteral of string
  | Uninit
[@@deriving eq, show]

let unwrap_reg_thunk ~default = function Reg reg -> reg | _ -> default ()
let unwrap_rel_thunk ~default = function Rel rel -> rel | _ -> default ()

let unwrap_base_offset_thunk ~default = function
  | Base_offset (rel, reg) -> (rel, reg)
  | _ -> default ()

let unwrap_string_literal_thunk ~default = function
  | StringLiteral str -> str
  | _ -> default ()

let unwrap_reg_exn = function Reg reg -> reg | _ -> raise Unwrap_failed
let unwrap_rel_exn = function Rel rel -> rel | _ -> raise Unwrap_failed

let unwrap_base_offset_exn = function
  | Base_offset (rel, reg) -> (rel, reg)
  | _ -> raise Unwrap_failed

let unwrap_string_literal_exn = function
  | StringLiteral str -> str
  | _ -> raise Unwrap_failed
