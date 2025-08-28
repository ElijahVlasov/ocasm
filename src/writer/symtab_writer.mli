open Base
open Ocasm_bfd.Bfd
open Ocasm_assembler
open Utils

val set_symtab :
  'a Word_type.t ->
  (Section.t, 'a Section_writer.section_info) Hashtbl.t ->
  'a Symbol.t list ->
  unit BfdMonad.t
