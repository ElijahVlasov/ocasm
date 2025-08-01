open! Import

module type S0 = sig
  type t

  val next : t -> char
  val peek : t -> char
  val close : t -> unit
end

module type S = sig
  include S0

  val skip : t -> unit
  val next_n_times : n:int -> t -> char
  val skip_n_times : n:int -> t -> unit
  val read_buf : t -> Buffer.t -> unit
end

module MkS (S0 : S0) : S with type t := S0.t

type 'a t = (module S with type t = 'a)

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
  type t = Input.t Positioned.t

  include S with type t := t
  include Positioned.S0 with type t := t

  val create : Input.t -> t
end
