open Import

type ('a, 'h, 't) t

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
  (module Isa_token.S with type t = 't) ->
  'a Input.t ->
  'a ->
  (module Diagnostics_handler.S with type t = 'h) ->
  'h ->
  ('a, 'h, 't) t

module Token_info : sig
  type t = { starts : Location.t; ends : Location.t; string : unit -> string }

  include Equal.S with type t := t
  include To_string.S with type t := t
end

val next_token : ('a, 'h, 't) t -> ('t Token.t * Token_info.t) option
val to_seq : ('a, 'h, 't) t -> ('t Token.t * Token_info.t) option Sequence.t
val to_list : ('a, 'h, 't) t -> ('t Token.t * Token_info.t) list option
