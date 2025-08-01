open Ctypes
open Ocasm_utils

type 'a carray

val of_list : 'a Word_type.t -> 'a list -> 'a carray
val start : 'a carray -> 'a ptr
