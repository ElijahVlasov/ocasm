open Base
open Ocasm_binary
open Ocasm_binary.Bfd
open Ocasm_assembler

val set_symtab :
  'a Word_type.t ->
  (Section.t, 'a Section_writer.section_info) Hashtbl.t ->
  'a Symbol.t list ->
  unit BfdMonad.t
