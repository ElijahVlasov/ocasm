open! Import
include Input_intf.Intf

(** An input type that observes a string. *)
module StringInput : sig
  include S

  val create : string -> t
end

(** An input type that observes a file. *)
module FileInput : sig
  include S

  val create : Path.t -> t
end

val with_input : 'a t -> 'a -> f:('a -> 'b) -> 'b
(** Given an input and its module type performs a computation [f] making sure
    that the input is properly closed at the computation end/exception. *)
