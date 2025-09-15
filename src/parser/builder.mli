open! Import

module Mk (I : sig
  type register

  module Reg : sig
    type t := register

    val bit_size : t -> int
  end

  type reloc_data
end) : sig
  open I

  type ('comm, 'a) t

  val create :
    ?len:int ->
    'comm Isa.Expr.t ->
    word_size:int ->
    builder_fn:(register, 'comm, reloc_data, 'a) Builder_fn.t ->
    ('comm, 'a) t

  val add_register :
    ('comm, 'a) t -> register -> (unit, Diagnostics.Error.t) Result.t

  val add_rel :
    ('comm, 'a) t ->
    reloc_data Relocatable.t ->
    (unit, Diagnostics.Error.t) Result.t

  val add_string :
    ('comm, 'a) t -> string -> (unit, Diagnostics.Error.t) Result.t

  val add_base_offset :
    ('comm, 'a) t ->
    register ->
    reloc_data Relocatable.t ->
    (unit, Diagnostics.Error.t) Result.t

  val start : ('comm, 'a) t -> 'comm -> unit
  val build : ('comm, 'a) t -> 'a
end
