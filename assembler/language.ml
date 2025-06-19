open Base
open Ocasm_binary

module type WORD_TYPE = sig
  type word

  val word_type : word Word_type.t
end

module Identifier : sig
  type t

  include Hashtbl.Key.S with type t := t
  include Stringable.S with type t := t
end = struct
  include String
end

module type I = sig
  type t

  include WORD_TYPE

  val assemble : t -> sym_tab:(Identifier.t, word) Hashtbl.t -> word
end

module Relocateable (WordType : WORD_TYPE) = struct
  type t = Abs of WordType.word | Rel of Identifier.t
end

module type A = sig
  type instruction
  type directive
  type state
end

module type S = sig
  include A

  type expr =
    | Directive of directive
    | Instruction of instruction
    | Label of Identifier.t

  val assemble_expr : state -> expr -> unit
  val init_state : state
  val finalize_state : state -> (Section.t, Buffer.t) Hashtbl.t
end

module type M = sig
  include A

  val init_state : state
  val finalize_state : state -> (Section.t, Buffer.t) Hashtbl.t
  val assemble_directive : state -> directive -> unit
  val assemble_instruction : state -> instruction -> unit
end

module Make (M : M) : S = struct
  include M

  type expr =
    | Directive of M.directive
    | Instruction of M.instruction
    | Label of Identifier.t

  let assemble_expr s expr =
    match expr with
    | Directive dir -> M.assemble_directive s dir
    | Instruction instr -> M.assemble_instruction s instr
    | Label label -> failwith "Not implemented"
end
