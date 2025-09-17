open! Import
module Argument = Argument
module Builder_fn = Builder_fn

module Mk (I : sig
  module Reg : Isa.Register.S

  type reloc_data
  type structured_instruction

  module Opcode : sig
    include Isa.Expr.S

    val build : (Reg.t, t, reloc_data, structured_instruction) Builder_fn.t
  end

  type structured_directive

  module Directive : sig
    include Isa.Expr.S

    val build : (Reg.t, t, reloc_data, structured_directive) Builder_fn.t
  end

  module Reserved : sig
    include Isa.Expr.S

    val build : (Reg.t, t, reloc_data, reloc_data Relocatable.t) Builder_fn.t
  end
end) : sig
  open I

  type t

  val next :
    t -> (structured_instruction, structured_directive) Command.t option

  val to_seq :
    t -> (structured_instruction, structured_directive) Command.t Sequence.t

  val to_list :
    t -> (structured_instruction, structured_directive) Command.t list

  val create :
    ?path:Path.t ->
    word_size:int ->
    ((Reg.t, Directive.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
    * Lexer.Token_info.t)
    Sequence.t ->
    Diagnostics_printer.t ->
    t

  val no_errors : t -> bool
end
