open Base
open Ocasm_binary

module type WORD_TYPE = sig
  type word

  val wt : word Word_type.t
end

module Identifier : sig
  type t

  include Hashtbl.Key.S with type t := t
  include Stringable.S with type t := t
end = struct
  include String
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

module Make (M : M) = struct
  include M

  type expr =
    | Directive of M.directive
    | Instruction of M.instruction
    | Label of string

  let assemble_expr s expr =
    match expr with
    | Directive dir -> M.assemble_directive s dir
    | Instruction instr -> M.assemble_instruction s instr
    | Label label ->
        let sec_s = State.section_state s in
        let stab_s = State.symtab_state s in
        let loc = Section_state.loc_counter sec_s in
        let section = Section_state.curr_section sec_s in
        let flags = Bfd.Symbol_flags.bsf_global in
        Symtab_state.add_symbol stab_s
          { name = label; value = Word_type.of_int wt loc; section; flags }
end

module type I = sig
  type t

  include WORD_TYPE

  val assemble : word State.t -> t -> unit
end

module Make2 (I : I) = Make (struct
  type instruction = I.t
  type directive = Directive.t

  include I

  let assemble_directive = Directive.assemble_directive
  let assemble_instruction = I.assemble
end)
