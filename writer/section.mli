open Base

type t = Data | Text | Bss [@@deriving eq, ord, hash, sexp]

include Equal.S with type t := t

val to_string : t -> string
val section_flags : t -> Bfd.Section_flags.t
