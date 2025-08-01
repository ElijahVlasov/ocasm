open Base

type t = ROData | Text | Data | Bss [@@deriving eq, ord, hash, compare, sexp]

include Equal.S with type t := t

val to_string : t -> string
