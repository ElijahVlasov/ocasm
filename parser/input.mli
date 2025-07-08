open Base

module type S = sig
  type t

  val next : t -> char
  val peek : t -> char
  val skip : t -> unit
  val close : t -> unit

  module Cursor : sig
    type parent_t := t
    type t

    val parent : t -> parent_t
    val next : t -> char option
    val step : t -> bool
    val step_unchecked : t -> unit
    val back : t -> bool
    val back_unchecked : t -> unit
  end

  val start : t -> Cursor.t
  val get : t -> Cursor.t -> char
  val advance : t -> Cursor.t -> unit
end

type 'a t = (module S with type t = 'a)

val eof : char

module StringInput : sig
  include S

  val create : content:string -> t
end

module FileInput : sig
  include S

  val create : path:string -> t
end

val with_input : 'a t -> 'a -> f:('a -> 'b) -> 'b

module MakePos (Input : S) : sig
  include S

  val create : Input.t -> t
  val pos : t -> int * int
end
