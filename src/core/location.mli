open! Import

type t = private int * int

val create : int -> int -> t

include Default.S with type t := t
include Equal.S with type t := t
include Pretty_printer.S with type t := t
