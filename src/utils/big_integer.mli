open! Import

type t [@@deriving eq, show]

exception Too_big_for_int64

val zero : t
val of_int : int -> t
val of_int32 : int32 -> t
val of_int64 : int64 -> t
val of_buffer : Buffer.t -> t
val to_int64_exn : t -> int64
