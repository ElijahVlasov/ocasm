open! Import

type ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t

val create :
  ?path:Path.t ->
  'reg Isa.Register.t ->
  'dir Isa.Expr.t ->
  'opcode Isa.Expr.t ->
  'res Isa.Expr.t ->
  word_size:int ->
  build_instruction:('reg, 'opcode, 'rel, 'out) Builder.Builder_fn.t ->
  build_directive:('reg, 'dir, 'rel, 'out) Builder.Builder_fn.t ->
  build_reserved:('reg, 'res, 'rel, 'rel Relocatable.t) Builder.Builder_fn.t ->
  (('reg, 'dir, 'opcode, 'res) Isa.Token.t Token.t * Lexer.Token_info.t)
  Sequence.t ->
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t

val with_opcode_builder :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  'opcode ->
  (('reg, 'opcode, 'rel, 'out) Builder.t -> 'a) ->
  'a

val with_dir_builder :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  'dir ->
  (('reg, 'dir, 'rel, 'out) Builder.t -> 'a) ->
  'a

val add_register :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  ('reg, 'comm, 'rel, 'out) Builder.t ->
  'reg ->
  unit

val add_rel :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  ('reg, 'comm, 'rel, 'out) Builder.t ->
  'rel Relocatable.t ->
  unit

val add_string :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  ('reg, 'comm, 'rel, 'out) Builder.t ->
  string ->
  unit

val add_base_offset :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  ('reg, 'comm, 'rel, 'out) Builder.t ->
  'reg ->
  'rel Relocatable.t ->
  unit

val build :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  ('reg, 'comm, 'rel, 'out) Builder.t ->
  'out

val next :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  ('reg, 'dir, 'opcode, 'res) Isa.Token.t Token.t

val peek :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  ('reg, 'dir, 'opcode, 'res) Isa.Token.t Token.t

val skip : ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t -> unit

val next_non_whitespace :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  ('reg, 'dir, 'opcode, 'res) Isa.Token.t Token.t

val skip_whitespaces_and_newlines :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t -> unit

val peek_non_whitespace :
  ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t ->
  ('reg, 'dir, 'opcode, 'res) Isa.Token.t Token.t

val last_token_info : ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t -> Token_info.t
