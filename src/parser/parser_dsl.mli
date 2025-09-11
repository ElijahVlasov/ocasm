open! Import

module Mk
    (Opcode : Isa.Expr.S)
    (Direc : Isa.Expr.S)
    (Reserved : Isa.Expr.S)
    (Reg : Isa.Register.S)
    (Reloc_data : T.T) : sig
  type 'out t

  val create :
    ?path:Path.t ->
    word_size:int ->
    build_instruction:(Reg.t, Opcode.t, Reloc_data.t, 'out) Builder.Builder_fn.t ->
    build_directive:(Reg.t, Direc.t, Reloc_data.t, 'out) Builder.Builder_fn.t ->
    build_reserved:
      ( Reg.t,
        Reserved.t,
        Reloc_data.t,
        Reloc_data.t Relocatable.t )
      Builder.Builder_fn.t ->
    ((Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
    * Lexer.Token_info.t)
    Sequence.t ->
    Diagnostics_printer.t ->
    'out t

  val with_opcode_builder :
    'out t ->
    Opcode.t ->
    ((Reg.t, Opcode.t, Reloc_data.t, 'out) Builder.t -> 'a) ->
    'a

  val with_dir_builder :
    'out t ->
    Direc.t ->
    ((Reg.t, Direc.t, Reloc_data.t, 'out) Builder.t -> 'a) ->
    'a

  val add_register :
    'out t -> (Reg.t, 'comm, Reloc_data.t, 'out) Builder.t -> Reg.t -> unit

  val add_rel :
    'out t ->
    (Reg.t, 'comm, Reloc_data.t, 'out) Builder.t ->
    Reloc_data.t Relocatable.t ->
    unit

  val add_string :
    'out t -> (Reg.t, 'comm, Reloc_data.t, 'out) Builder.t -> string -> unit

  val add_base_offset :
    'out t ->
    (Reg.t, 'comm, Reloc_data.t, 'out) Builder.t ->
    Reg.t ->
    Reloc_data.t Relocatable.t ->
    unit

  val build : 'out t -> (Reg.t, 'comm, Reloc_data.t, 'out) Builder.t -> 'out
  val next : _ t -> (Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
  val peek : _ t -> (Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
  val skip : _ t -> unit

  val next_non_whitespace :
    _ t -> (Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t

  val skip_whitespaces_and_newlines : _ t -> unit

  val peek_non_whitespace :
    _ t -> (Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t

  val last_token_info : _ t -> Token_info.t
end
