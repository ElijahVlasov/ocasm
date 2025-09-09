open! Import
module Argument = Argument
module Builder_fn = Builder.Builder_fn

type ('reg, 'dir, 'opcode, 'res, 'rel, 'instr, 'dir_ast) t

val next : (_, _, _, _, _, 'instr, 'dir_ast) t -> ('instr, 'dir_ast) Command.t

val to_seq :
  (_, _, _, _, _, 'instr, 'dir_ast) t -> ('instr, 'dir_ast) Command.t Sequence.t

val to_list :
  (_, _, _, _, _, 'instr, 'dir_ast) t -> ('instr, 'dir_ast) Command.t list

val create :
  ?path:Path.t ->
  'reg Isa.Register.t ->
  'dir Isa.Expr.t ->
  'opcode Isa.Expr.t ->
  'res Isa.Expr.t ->
  word_size:int ->
  build_instruction:('reg, 'opcode, 'rel, 'instr) Builder.Builder_fn.t ->
  build_directive:('reg, 'dir, 'rel, 'dir_ast) Builder.Builder_fn.t ->
  build_reserved:('reg, 'res, 'rel, 'rel Relocatable.t) Builder.Builder_fn.t ->
  (('reg, 'dir, 'opcode, 'res) Isa.Token.t Token.t * Lexer.Token_info.t)
  Sequence.t ->
  Diagnostics_printer.t ->
  ('reg, 'dir, 'opcode, 'res, 'rel, 'instr, 'dir_ast) t

val no_errors : (_, _, _, _, _, _, _) t -> bool
