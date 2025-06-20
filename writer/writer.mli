open Base
open Ocasm_binary
open Ocasm_assembler

val write_object_file :
  word_type:'a Word_type.t ->
  file_name:string ->
  sections:(Section.t * 'a list) list ->
  symtab:'a Symbol.t list ->
  unit
