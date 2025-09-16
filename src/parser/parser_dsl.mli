open! Import

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

  module Builder : sig
    type ('comm, 'out) t
  end

  type t

  type 'a result =
    ('a, (Diagnostics.Error.t, Diagnostics.Warning.t) Either.t) Result.t

  val create :
    ?path:Path.t ->
    word_size:int ->
    ((Reg.t, Directive.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
    * Lexer.Token_info.t)
    Sequence.t ->
    Diagnostics_printer.t ->
    t

  val with_opcode_builder :
    t -> Opcode.t -> ((Opcode.t, structured_instruction) Builder.t -> 'a) -> 'a

  val with_dir_builder :
    t ->
    Directive.t ->
    ((Directive.t, structured_directive) Builder.t -> 'a) ->
    'a

  val add_register : t -> ('comm, 'out) Builder.t -> Reg.t -> unit result

  val add_rel :
    t -> ('comm, 'out) Builder.t -> reloc_data Relocatable.t -> unit result

  val add_string : t -> ('comm, 'out) Builder.t -> string -> unit result

  val add_base_offset :
    t ->
    ('comm, 'out) Builder.t ->
    Reg.t ->
    reloc_data Relocatable.t ->
    unit result

  val build : t -> ('comm, 'out) Builder.t -> 'out
  val next : t -> (Reg.t, Directive.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
  val peek : t -> (Reg.t, Directive.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
  val skip : t -> unit

  val next_non_whitespace :
    t -> (Reg.t, Directive.t, Opcode.t, Reserved.t) Isa.Token.t Token.t

  val skip_whitespaces_and_newlines : t -> unit

  val peek_non_whitespace :
    t -> (Reg.t, Directive.t, Opcode.t, Reserved.t) Isa.Token.t Token.t

  val last_token_info : t -> Token_info.t
  val warning : t -> Diagnostics.Warning.t -> unit result
  val error : t -> Diagnostics.Error.t -> 'a result
end
