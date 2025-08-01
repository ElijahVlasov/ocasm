open! Import
include Input_intf.Intf

module StringInput : sig
  include S

  val create : content:string -> t
end

module FileInput : sig
  include S

  val create : path:string -> t
end

val with_input : 'a t -> 'a -> f:('a -> 'b) -> 'b
