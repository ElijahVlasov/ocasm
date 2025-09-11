open! Import
include module type of Builder_intf

module Mk (Reg : Isa.Register.S) (Reloc_data : T.T) : sig
  type ('comm, 'a) t

  val create :
    ?len:int ->
    'comm Isa.Expr.t ->
    word_size:int ->
    builder_fn:(Reg.t, 'comm, Reloc_data.t, 'a) Builder_fn.t ->
    ('comm, 'a) t

  val add_register : ('comm, 'a) t -> Reg.t -> unit
  val add_rel : ('comm, 'a) t -> Reloc_data.t Relocatable.t -> unit
  val add_string : ('comm, 'a) t -> string -> unit

  val add_base_offset :
    ('comm, 'a) t -> Reg.t -> Reloc_data.t Relocatable.t -> unit

  val start : ('comm, 'a) t -> 'comm -> unit
  val build : ('comm, 'a) t -> 'a
end
