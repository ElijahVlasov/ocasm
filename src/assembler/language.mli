open Base
open Ocasm_utils

module type A = sig
  type instruction
  type directive
end

module type E = sig
  include A

  type expr =
    | Directive of directive
    | Instruction of instruction
    | Label of string
end

module type S = sig
  include Word_type.W
  include E

  val assemble_expr : word State.t -> expr -> unit
end

module type M = sig
  include A
  include Word_type.W

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

  include Word_type.W

  val assemble : word State.t -> t -> unit
end

module Make2 (I : I) :
  S
    with type word := I.word
    with type instruction := I.t
    with type directive := Directive.t
