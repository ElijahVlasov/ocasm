open Base

type 'a t

val create : 'a Input.t -> 'a -> 'a t
val next_token : 'a t -> Token.t
val to_seq : 'a t -> Token.t Sequence.t
val to_list : 'a t -> Token.t list
