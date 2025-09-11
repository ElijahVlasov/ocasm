open! Import
module Argument = Argument
module Builder_fn = Builder_fn

module Mk
    (Opcode : Isa.Expr.S)
    (Direc : Isa.Expr.S)
    (Reserved : Isa.Expr.S)
    (Reg : Isa.Register.S)
    (Reloc_data : T.T) : sig
  type ('instr, 'dir_ast) t

  val next : ('instr, 'dir_ast) t -> ('instr, 'dir_ast) Command.t
  val to_seq : ('instr, 'dir_ast) t -> ('instr, 'dir_ast) Command.t Sequence.t
  val to_list : ('instr, 'dir_ast) t -> ('instr, 'dir_ast) Command.t list

  val create :
    ?path:Path.t ->
    word_size:int ->
    build_instruction:(Reg.t, Opcode.t, Reloc_data.t, 'instr) Builder_fn.t ->
    build_directive:(Reg.t, Direc.t, Reloc_data.t, 'dir_ast) Builder_fn.t ->
    build_reserved:
      (Reg.t, Reserved.t, Reloc_data.t, Reloc_data.t Relocatable.t) Builder_fn.t ->
    ((Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token.t
    * Lexer.Token_info.t)
    Sequence.t ->
    Diagnostics_printer.t ->
    ('instr, 'dir_ast) t

  val no_errors : (_, _) t -> bool
end
