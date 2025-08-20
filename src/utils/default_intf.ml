module type S = sig
  type t

  val default : unit -> t
end
