open Base
open Bfd.CArray
module Section = Section

type 'a symbol = 'a Symbol.symbol

val write_object_file :
  word_type:'a word_type ->
  name:string ->
  sections:(Section.t * 'a list) list ->
  symtab:'a symbol list ->
  unit
