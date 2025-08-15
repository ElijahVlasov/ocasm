open! Import

type ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t

val create :
  ?path:Path.t ->
  'reg Isa.Register.t ->
  'dir Isa.Expr.t ->
  'opcode Isa.Expr.t ->
  'res Isa.Expr.t ->
  word_size:int ->
  build_instruction:('reg, 'opcode, 'rel, 'out) Builder.Builder_fn.t ->
  build_directive:('reg, 'dir, 'rel, 'out) Builder.Builder_fn.t ->
  build_reserved:('reg, 'res, 'rel, 'out) Builder.Builder_fn.t ->
  ('tok Token.t * Lexer.Token_info.t) option Sequence.t ->
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t

val with_opcode_builder :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t ->
  'opcode ->
  (('reg, 'opcode, 'rel, 'out) Builder.t -> 'a) ->
  'a

val with_dir_builder :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t ->
  'dir ->
  (('reg, 'dir, 'rel, 'out) Builder.t -> 'a) ->
  'a

val add_register :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t ->
  ('reg, 'comm, 'rel, 'out) Builder.t ->
  'reg ->
  unit

val add_rel :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t ->
  ('reg, 'comm, 'rel, 'out) Builder.t ->
  'rel Relocatable.t ->
  unit

val add_string :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t ->
  ('reg, 'comm, 'rel, 'out) Builder.t ->
  string ->
  unit

val add_base_offset :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t ->
  ('reg, 'comm, 'rel, 'out) Builder.t ->
  'reg ->
  'rel Relocatable.t ->
  unit

val build :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t ->
  ('reg, 'comm, 'rel, 'out) Builder.t ->
  'out

val next :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t ->
  ('tok Token.t * Lexer.Token_info.t) option
