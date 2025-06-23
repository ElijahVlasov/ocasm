open Base
open Ocasm_binary

module type WORD_TYPE = sig
  type word

  val wt : word Word_type.t
end

module type A = sig
  type instruction
  type directive
end

module type S = sig
  include A
  include WORD_TYPE

  type expr =
    | Directive of directive
    | Instruction of instruction
    | Label of string

  val assemble_expr : word State.t -> expr -> unit
end

module type M = sig
  include A
  include WORD_TYPE

  val assemble_directive : word State.t -> directive -> unit
  val assemble_instruction : word State.t -> instruction -> unit
end

module Make (M : M) :
  S
    with type word := M.word
    with type instruction := M.instruction
    with type directive := M.directive

module type I = sig
  type t

  include WORD_TYPE

  val assemble : word State.t -> t -> unit
end

module Make2 (I : I) :
  S
    with type word := I.word
    with type instruction := I.t
    with type directive := Directive.t


