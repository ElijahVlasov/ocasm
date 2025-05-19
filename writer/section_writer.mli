open Base
open Bfd
open Bfd.CArray

type 'a section_info = { bfd_sec : Bfd.asection; contents : 'a list }

val init_sections :
  'a word_type ->
  (Section.t * 'a list) list ->
  (Section.t, 'a section_info) Hashtbl.t BfdMonad.t

val write_section_contents :
  'a word_type -> (Section.t, 'a section_info) Hashtbl.t -> unit BfdMonad.t
