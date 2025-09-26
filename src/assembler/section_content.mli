open! Import

module Mk (I : sig
  type instruction

  val byte_size : instruction -> int
end) : sig
  type t

  val create : int -> Section.t -> t
  val add_dir : t -> Directive.t -> unit
  val add_instr : t -> I.instruction -> unit
  val name : t -> Section.t
  val curr_offset : t -> int
  val length : t -> int

  val iter :
    t ->
    f:(int -> (I.instruction, Directive.t) Preprocessed_command.t -> unit) ->
    unit
end
