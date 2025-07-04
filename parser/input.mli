open Base

module type S = sig
  type t

  val next : t -> char
  val peek : t -> char
  val skip : t -> unit

  val close : t -> unit
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
