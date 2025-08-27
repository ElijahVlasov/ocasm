open Import
include module type of Ocasm_lexer_intf

type ('a, 'h, 't) t
(** The lexer type.

    {3 Type parameters}
    - ['a] is for input type.
    - ['h] is for diagnostics handler type.
    - ['t] is for ISA-specific token type. *)

val create :
  't Isa_token.t ->
  'a Input.t ->
  'a ->
  (module Diagnostics_handler.S with type t = 'h) ->
  'h ->
  ('a, 'h, 't) t
(** Create a lexer *)

val next_token : (_, _, 't) t -> ('t Token.t * Token_info.t) option
(** Returns the next token with its meta information or, if the lexer failed,
    return [None].*)

val to_seq : (_, _, 't) t -> ('t Token.t * Token_info.t) option Sequence.t
(** Converts a lexer into a sequence obtained by repetitive application of
    [next_token]. *)

val to_list : (_, _, 't) t -> ('t Token.t * Token_info.t) list option
(** Converts a lexer into a list obtained by repetitive application of
    [next_token]. *)
