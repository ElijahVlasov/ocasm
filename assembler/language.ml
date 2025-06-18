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

module Assembly (Instruction : I) = struct
  type t =
    | Directive of Directives.t
    | Instruction of Instruction.t
    | Label of Identifier.t
end

module Relocateable (WordType : WORD_TYPE) = struct
  type t = Abs of WordType.word | Rel of Identifier.t
end

module Assembler (Instruction : I) = struct
  module Lang = Assembly (Instruction)

  let assemble (line : Lang.t) =
    let state = State.create [ Section.Text; Section.Data; Section.Bss ] in
    match line with
    | Lang.Directive dir -> failwith "Not implemented"
    | Lang.Instruction instr ->
        failwith "Not implemented"
        (* Instruction.assemble ~sym_tab:state.sym_tab instr *)
    | Lang.Label label -> failwith "Not implemented"
end
