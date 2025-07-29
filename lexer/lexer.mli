open Base
open Ocasm_utils

type ('a, 't) t

module Isa_token : sig
  module type S = sig
    type t

    val directive : string -> t option
    val name : string -> t option
    val reserved : string -> t option

    include To_string.S with type t := t
    include Equal.S with type t := t
  end
end

val create :
  ?diagnostics:Diagnostics.t ->
  (module Isa_token.S with type t = 't) ->
  'a Input.t ->
  'a ->
  ('a, 't) t

type token_info = {
  starts : int * int;
  ends : int * int;
  string : unit -> string;
}

val next_token : ('a, 't) t -> ('t Token.t * token_info) option
val to_seq : ('a, 't) t -> ('t Token.t * token_info) option Sequence.t
val to_list : ('a, 't) t -> ('t Token.t * token_info) list option
