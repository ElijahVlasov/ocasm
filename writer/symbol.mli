open Base
open Bfd
open Bfd.CArray
open Section_writer

type 'a symbol = {
  section : Section.t;
  name : string;
  value : 'a;
  flags : Bfd.Symbol_flags.t;
}

val set_symtab :
  'a word_type ->
  (Section.t, 'a section_info) Hashtbl.t ->
  'a symbol list ->
  unit BfdMonad.t
