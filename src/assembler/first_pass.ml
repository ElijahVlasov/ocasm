open! Import

module Mk (I : sig
  type pseudo_instruction
  type instruction

  val prep_instruction : pseudo_instruction -> instruction
end) =
struct
  type t = {
    sections : (string, unit) Hashtbl.t;
    symtab : (string, unit) Hashtbl.t;
    mutable curr_sec : string;
  }
end
