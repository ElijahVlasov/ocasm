open! Import
include module type of Builder_intf

type ('reg, 'comm, 'rel, 'a) t

val create :
  ?len:int ->
  'reg Isa.Register.t ->
  'comm Isa.Expr.t ->
  word_size:int ->
  builder_fn:('reg, 'comm, 'rel, 'a) Builder_fn.t ->
  ('reg, 'comm, 'rel, 'a) t

val add_register : ('reg, 'comm, 'rel, 'a) t -> 'reg -> unit
val add_rel : ('reg, 'comm, 'rel, 'a) t -> 'rel Relocatable.t -> unit
val add_string : ('reg, 'comm, 'rel, 'a) t -> string -> unit

val add_base_offset :
  ('reg, 'comm, 'rel, 'a) t -> 'reg -> 'rel Relocatable.t -> unit

val start : ('reg, 'comm, 'rel, 'a) t -> 'comm -> unit
val build : ('reg, 'comm, 'rel, 'a) t -> 'a
