open! Import

type t =
  | Ascii of string list
  | Asciiz of string list
  | Byte of bytes
  | Skip of int
  | Section of Section.t
  (* Should this involve word type stuff? *)
  | Word of int32 list
