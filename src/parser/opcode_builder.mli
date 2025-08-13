open! Import

type ('reg, 'opcode, 'rel) t

val create :
  ?len:int ->
  'reg Isa.Register.t ->
  'opcode Isa.Expr.t ->
  word_size:int ->
  ('reg, 'opcode, 'rel) t

val add_register : ('reg, 'opcode, 'rel) t -> 'reg -> unit
val add_rel : ('reg, 'opcode, 'rel) t -> 'rel Relocatable.t -> unit

val add_base_offset :
  ('reg, 'opcode, 'rel) t -> 'reg -> 'rel Relocatable.t -> unit

val start : ('reg, 'opcode, 'rel) t -> 'opcode -> unit

val build :
  ('reg, 'opcode, 'rel) t ->
  builder:('opcode -> ('reg, 'rel) Arg.t array -> 'a) ->
  'a
