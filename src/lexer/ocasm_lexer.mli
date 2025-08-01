open Import
include module type of Ocasm_lexer_intf

type ('a, 'h, 't) t
(** The lexer type.

    {3 Type parameters}
    - ['a] is for input type.
    - ['h] is for diagnostics handler type.
    - ['t] is for ISA-specific token type. *)

module Isa_token : sig
  module type S = sig
    type t
    (** This type is supposed to be the type of ISA-specific tokens. I.e.
        registers, instruction names, etc. *)

    val directive : string -> t option
    (** Tries to parse a directive (something that starts with ['.']). *)

    val name : string -> t option
    (** Tries to parse an ISA-name (register, instruction, etc). *)

    val reserved : string -> t option
    (** Tries to parse a resrved keyword (something that starts with ['%']). *)

    include To_string.S with type t := t
    include Equal.S with type t := t
  end
end

(**/**)

module Token_info : sig
  type t = {
    starts : Location.t;  (** Here the token starts *)
    ends : Location.t;  (** Here the token ends *)
    string : unit -> string;
        (** The physical representation of the token in the source file. *)
  }

  include Equal.S with type t := t
  include To_string.S with type t := t
end

val create :
  (module Isa_token.S with type t = 't) ->
  'a Input.t ->
  'a ->
  (module Diagnostics_handler.S with type t = 'h) ->
  'h ->
  ('a, 'h, 't) t
(** Create a lexer *)

val next_token : ('a, 'h, 't) t -> ('t Token.t * Token_info.t) option
(** Returns the next token with its meta information or, if the lexer failed,
    return [None].*)

val to_seq : ('a, 'h, 't) t -> ('t Token.t * Token_info.t) option Sequence.t
(** Converts a lexer into a sequence obtained by repetitive application of
    [next_token]. *)

val to_list : ('a, 'h, 't) t -> ('t Token.t * Token_info.t) list option
(** Converts a lexer into a list obtained by repetitive application of
    [next_token]. *)
