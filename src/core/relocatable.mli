open! Import
include module type of Relocatable_intf

val imm : int -> int64 -> 'a t
val name : string -> 'a t
val reloc : int -> int64 -> 'a -> 'a t
val of_big_integer : Big_integer.t -> 'a t
