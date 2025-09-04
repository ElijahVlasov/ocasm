open Import
include module type of Lexer_intf

type ('a, 't) t
(** The lexer type.

    {2 Type parameters}
    - ['a] is for input type.
    - ['t] is for ISA-specific token type. *)

val create :
  't Isa_token.t -> 'a Input.t -> 'a -> Diagnostics_printer.t -> ('a, 't) t
(** Create a lexer *)

val next : (_, 't) t -> 't Token.t * Token_info.t
(** Returns the next token with its meta information or, if the lexer failed,
    return [None].*)

val to_seq : (_, 't) t -> ('t Token.t * Token_info.t) Sequence.t
(** Converts a lexer into a sequence obtained by repetitive application of
    [next_token]. *)

val to_list : (_, 't) t -> ('t Token.t * Token_info.t) list
(** Converts a lexer into a list obtained by repetitive application of
    [next_token]. *)

val no_errors : (_, _) t -> bool
