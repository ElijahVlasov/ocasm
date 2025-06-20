open Base
open Bfd
open Bfd.CArray
open Ocasm_assembler

val set_symtab :
  'a word_type ->
  (Section.t, 'a Section_writer.section_info) Hashtbl.t ->
  'a Symbol.t list ->
  unit BfdMonad.t
