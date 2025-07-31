open Base

type t = private int * int

val create : int -> int -> t

include Equal.S with type t := t

val pp : Formatter.t -> t -> unit
