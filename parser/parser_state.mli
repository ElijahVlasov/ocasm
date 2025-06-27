open Base

module type S = sig
  type t

  val consume : t -> (string * char) option
end
