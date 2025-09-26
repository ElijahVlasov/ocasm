open! Import

module Mk (I : sig
  type pseudo_instruction
  type instruction

  val prep_instruction : pseudo_instruction -> instruction list
  val byte_size : instruction -> int
end) : sig
  type t

  module Section_content : sig
    type t

    val length : t -> int

    val iter :
      t ->
      f:(int -> (I.instruction, Directive.t) Preprocessed_command.t -> unit) ->
      unit
  end

  val create : int -> t
  val consume : t -> (I.pseudo_instruction, Directive.t) Command.t -> unit
  val symtab : t -> (string, Symbol.t) Hashtbl.t
  val sections : t -> (Section.t, Section_content.t) Hashtbl.t
end
