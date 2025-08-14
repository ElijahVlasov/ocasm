open! Import

module Arg = struct
  type ('reg, 'rel) t =
    | Reg of 'reg
    | Rel of 'rel Relocatable.t
    | Base_offset of 'rel Relocatable.t * 'reg
    | StringLiteral of string
    | Uninit
end

module Builder_fn = struct
  type ('reg, 'comm, 'rel, 'a) t = 'comm -> ('reg, 'rel) Arg.t array -> 'a
end
