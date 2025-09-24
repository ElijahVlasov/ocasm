open! Import

type t =
  | Ascii of string list
  | Asciiz of string list
  | Byte of bytes
  | Skip of int
  | Section of Section.t
  | Word32 of int32 list

let byte_size = function
  | Ascii strs ->
      List.fold strs ~init:0 ~f:(fun acc str -> acc + String.length str)
  | Asciiz strs ->
      List.fold strs ~init:0 ~f:(fun acc str -> acc + String.length str + 1)
  | Byte bs -> Bytes.length bs
  | Skip off -> off
  | Section _ -> 0
  | Word32 lst -> 4 * List.length lst
