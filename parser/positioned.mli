open Base

module type S0 = sig
  type t

  val pos : t -> int * int
  val line : t -> int
  val col : t -> int

  include Equal.S with type t := t
end

module type S0_M = sig
  include S0

  val set_pos : t -> int -> int -> unit
end

module type F = sig
  include S0

  val step : t -> char -> unit
  val step_unchecked : t -> unit
end

module type S = sig
  include F

  val back : t -> char -> unit
  val back_unchecked : t -> unit
end

module MakePositionedForward (T : T) : sig
  type t

  include F with type t := t
  include S0_M with type t := t

  val create : ?line:int -> ?col:int -> T.t -> t
  val unwrap : t -> T.t
end

module MakePositioned (T : T) : sig
  type t

  include S with type t := t

  val create : ?line:int -> ?col:int -> T.t -> t
  val unwrap : t -> T.t
end
