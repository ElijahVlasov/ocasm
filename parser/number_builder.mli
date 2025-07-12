exception WrongDigit of char

module type S = sig
  type t

  val create : unit -> t
  val feed_exn : t -> char -> unit
  val build : t -> int64 array
end

module Bin_builder : S
module Oct_builder : S
module Dec_builder : S
module Hex_builder : S
