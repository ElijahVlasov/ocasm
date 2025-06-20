open Base
open Bfd.CArray
open Ocasm_assembler

val write_object_file :
  word_type:'a word_type ->
  file_name:string ->
  sections:(Section.t * 'a list) list ->
  symtab:'a Symbol.t list ->
  unit
