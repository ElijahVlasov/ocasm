open Ctypes
open Utils

type 'a carray

val of_list : 'a Word_type.t -> 'a list -> 'a carray
val start : 'a carray -> 'a ptr
