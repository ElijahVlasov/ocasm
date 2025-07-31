open Base

type t = private string

include Equal.S with type t := t

val of_string : string -> t
val to_string : t -> string
