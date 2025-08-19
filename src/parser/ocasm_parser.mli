open! Import

type ('reg, 'dir, 'opcode, 'res, 'rel, 'ast) t

val next : ('reg, 'dir, 'opcode, 'res, 'rel, 'ast) t -> 'ast option

val create :
  ?path:Path.t ->
  'reg Isa.Register.t ->
  'dir Isa.Expr.t ->
  'opcode Isa.Expr.t ->
  'res Isa.Expr.t ->
  word_size:int ->
  build_instruction:('reg, 'opcode, 'rel, 'ast) Builder.Builder_fn.t ->
  build_directive:('reg, 'dir, 'rel, 'ast) Builder.Builder_fn.t ->
  build_reserved:('reg, 'res, 'rel, 'ast) Builder.Builder_fn.t ->
  build_label:(string -> 'ast) ->
  (('reg, 'dir, 'opcode, 'res) Isa.Token.t Token.t * Lexer.Token_info.t) option
  Sequence.t ->
  ('reg, 'dir, 'opcode, 'res, 'rel, 'ast) t
