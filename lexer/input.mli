open! Import
include Input_intf.Intf

module StringInput : sig
  include S

  val create : string -> t
end

module FileInput : sig
  include S

  val create : Path.t -> t
end

val with_input : 'a t -> 'a -> f:('a -> 'b) -> 'b
