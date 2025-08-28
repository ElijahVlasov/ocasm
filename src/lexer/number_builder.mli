open Utils

exception WrongDigit of char

module type S = sig
  type t

  val is_digit : char -> bool
  val create : unit -> t
  val clear : t -> unit
  val add_char : t -> char -> unit
  val build : t -> Big_integer.t
end

module Bin_builder : S
module Oct_builder : S
module Dec_builder : S
module Hex_builder : S
