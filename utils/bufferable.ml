open Base

module type S = sig
  type t

  val add_to_buffer : Buffer.t -> t -> unit
end
