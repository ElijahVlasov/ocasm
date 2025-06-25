open Base
open Ocasm_assembler
open Ocasm_utils

val write_object_file :
  wt:'a Word_type.t ->
  file_name:string ->
  sections:(Section.t * 'a list) list ->
  symtab:'a Symbol.t list ->
  unit
