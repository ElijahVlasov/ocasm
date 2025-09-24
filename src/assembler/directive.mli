open! Import

type t =
  | Ascii of string list
  | Asciiz of string list
  | Byte of bytes
  | Skip of int
  | Section of Section.t
  (* Should this involve word type stuff? *)
  | Word32 of int32 list

val byte_size : t -> int
