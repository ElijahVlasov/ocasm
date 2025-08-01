open! Import

type 'a t [@@deriving eq]

val with_value : ('a -> 'b) -> 'a t -> 'b
val create : ?line:int -> ?col:int -> 'a -> 'a t
val pos : 'a t -> Location.t
val line : 'a t -> int
val col : 'a t -> int
val step : 'a t -> char -> unit
val step_unchecked : 'a t -> unit
val unwrap : 'a t -> 'a
