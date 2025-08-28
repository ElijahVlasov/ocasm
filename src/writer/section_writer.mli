open Base
open Ocasm_assembler
open Ocasm_bfd
open Utils

type 'a section_info = { bfd_sec : Bfd.asection; contents : 'a list }

val init_sections :
  'a Word_type.t ->
  (Section.t * 'a list) list ->
  (Section.t, 'a section_info) Hashtbl.t Bfd.BfdMonad.t

val write_section_contents :
  'a Word_type.t ->
  (Section.t, 'a section_info) Hashtbl.t ->
  unit Bfd.BfdMonad.t
