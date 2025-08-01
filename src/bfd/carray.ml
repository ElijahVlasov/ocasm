open Ctypes
open Ocasm_utils

type 'a carray = 'a Ctypes.carray

let word_type_to_typ : type a. a Word_type.t -> a Ctypes.typ = function
  | Word_type.Word32 -> Ctypes.int32_t
  | Word_type.Word64 -> Ctypes.int64_t

let of_list : type a. a Word_type.t -> a list -> a carray =
 fun wt list -> CArray.of_list (word_type_to_typ wt) list

let start = CArray.start
