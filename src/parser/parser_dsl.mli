open! Import

module Mk
    (Opcode : Isa.Expr.S)
    (Direc : Isa.Expr.S)
    (Reserved : Isa.Expr.S)
    (Reg : Isa.Register.S)
    (Reloc_data : T.T) : sig
  module Builder : sig
    type ('comm, 'out) t
  end

  type 'out t

  type 'a result =
    ('a, (Diagnostics.Error.t, Diagnostics.Warning.t) Either.t) Result.t

  val create :
    ?path:Path.t ->
    word_size:int ->
    build_instruction:(Reg.t, Opcode.t, Reloc_data.t, 'out) Builder_fn.t ->
    build_directive:(Reg.t, Direc.t, Reloc_data.t, 'out) Builder_fn.t ->
    build_reserved:
      (Reg.t, Reserved.t, Reloc_data.t, Reloc_data.t Relocatable.t) Builder_fn.t ->
    ((Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
    * Lexer.Token_info.t)
    Sequence.t ->
    Diagnostics_printer.t ->
    'out t

  val with_opcode_builder :
    'out t -> Opcode.t -> ((Opcode.t, 'out) Builder.t -> 'a) -> 'a

  val with_dir_builder :
    'out t -> Direc.t -> ((Direc.t, 'out) Builder.t -> 'a) -> 'a

  val add_register : 'out t -> ('comm, 'out) Builder.t -> Reg.t -> unit result

  val add_rel :
    'out t ->
    ('comm, 'out) Builder.t ->
    Reloc_data.t Relocatable.t ->
    unit result

  val add_string : 'out t -> ('comm, 'out) Builder.t -> string -> unit result

  val add_base_offset :
    'out t ->
    ('comm, 'out) Builder.t ->
    Reg.t ->
    Reloc_data.t Relocatable.t ->
    unit result

  val build : 'out t -> ('comm, 'out) Builder.t -> 'out
  val next : _ t -> (Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
  val peek : _ t -> (Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
  val skip : _ t -> unit

  val next_non_whitespace :
    _ t -> (Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t

  val skip_whitespaces_and_newlines : _ t -> unit

  val peek_non_whitespace :
    _ t -> (Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t

  val last_token_info : _ t -> Token_info.t
  val warning : _ t -> Diagnostics.Warning.t -> unit result
  val error : _ t -> Diagnostics.Error.t -> 'a result
end
