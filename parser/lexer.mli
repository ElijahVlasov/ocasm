open Base

type ('a, 't) t

module type T = sig
  type token

  val directive : string -> token option
  val name : string -> token option
  val reserved : string -> token option
end

type 't token = Common of Token.t | Isa_specific of 't [@@deriving eq]

val create : (module T with type token = 't) -> 'a Input.t -> 'a -> ('a, 't) t
val next_token : ('a, 't) t -> 't token
val to_seq : ('a, 't) t -> 't token Sequence.t
val to_list : ('a, 't) t -> 't token list
