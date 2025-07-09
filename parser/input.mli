open Base

module type C0 = sig
  type t

  val next : t -> char option
  val step : t -> bool
  val step_unchecked : t -> unit
  val back : t -> bool
  val back_unchecked : t -> unit
  val get : t -> char
end

module type C = sig
  include C0

  type input

  val create : input -> t
end

module type S = sig
  type t

  val next : t -> char
  val peek : t -> char
  val skip : t -> unit
  val close : t -> unit

  module Cursor : C with type input := t

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

module MakePositioned (Input : S) : sig
  type t

  module Cursor : sig
    include C with type input := t
    include Positioned.S0 with type t := t
  end

  include S with type t := t with module Cursor := Cursor
  include Positioned.S0 with type t := t

  val create : Input.t -> t
end
