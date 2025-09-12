open! Import
module Argument = Argument
module Builder_fn = Builder_fn

module Mk
    (Opcode : Isa.Expr.S)
    (Direc : Isa.Expr.S)
    (Reserved : Isa.Expr.S)
    (Reg : Isa.Register.S)
    (Reloc_data : T.T)
    (Struct_instr : T.T)
    (Struct_dir : T.T) : sig
  type t

  val next : t -> (Struct_instr.t, Struct_dir.t) Command.t
  val to_seq : t -> (Struct_instr.t, Struct_dir.t) Command.t Sequence.t
  val to_list : t -> (Struct_instr.t, Struct_dir.t) Command.t list

  val create :
    ?path:Path.t ->
    word_size:int ->
    build_instruction:
      (Reg.t, Opcode.t, Reloc_data.t, Struct_instr.t) Builder_fn.t ->
    build_directive:(Reg.t, Direc.t, Reloc_data.t, Struct_dir.t) Builder_fn.t ->
    build_reserved:
      (Reg.t, Reserved.t, Reloc_data.t, Reloc_data.t Relocatable.t) Builder_fn.t ->
    ((Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
    * Lexer.Token_info.t)
    Sequence.t ->
    Diagnostics_printer.t ->
    t

  val no_errors : t -> bool
end
