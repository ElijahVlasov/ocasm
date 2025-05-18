open Base
open Bfd.CArray

module Section : sig
  type t = Data | Text | Bss [@@deriving eq, ord, hash, sexp]

  include Equal.S with type t := t

  val to_string : t -> string
end

type 'a symbol = {
  section : Section.t;
  name : string;
  value : 'a;
  flags : Bfd.Symbol_flags.t;
}

val write_object_file :
  word_type:'a word_type ->
  name:string ->
  sections:(Section.t * 'a list) list ->
  symtab:'a symbol list ->
  unit
