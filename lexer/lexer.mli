open Base

type ('a, 't) t

module Isa_token : sig
  module type S = sig
    type t

    val directive : string -> t option
    val name : string -> t option
    val reserved : string -> t option
  end
end

val create :
  ?diagnostics:Diagnostics.t ->
  (module Isa_token.S with type t = 't) ->
  'a Input.t ->
  'a ->
  ('a, 't) t

val next_token : ('a, 't) t -> 't Token.t option
val to_seq : ('a, 't) t -> 't Token.t option Sequence.t
val to_list : ('a, 't) t -> 't Token.t list option
