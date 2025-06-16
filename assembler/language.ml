open Base
open Bfd.CArray

module type WORD_TYPE = sig
  type word

  val word_type : word word_type
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

  val assemble : t -> idens:(Identifier.t, word) Hashtbl.t -> word
end

module Assembly (Instruction : I) = struct
  type t = Directive of Directives.t | Instruction of Instruction.t
end

module Relocateable (WordType : WORD_TYPE) = struct
  type t = Abs of WordType.word | Rel of Identifier.t
end

module Assembler (Instruction : I) = struct
  module Lang = Assembly (Instruction)

  type state = {
    mutable section : Section.t;
    idens : (Identifier.t, Instruction.word) Hashtbl.t;
  }

  let assemble (line : Lang.t) =
    let state =
      { section = Section.Text; idens = Hashtbl.create (module Identifier) }
    in
    match line with
    | Lang.Directive dir -> failwith "Not implemented"
    | Lang.Instruction instr -> Instruction.assemble ~idens:state.idens instr
end
