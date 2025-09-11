open! Import
include module type of Lexer_intf

module Mk (Isa_token : Isa_token.S) (Input : Input.S) : sig
  type t
  (** The lexer type.

      {2 Type parameters}
      - ['a] is for input type.
      - [Isa_token.t] is for ISA-specific token type. *)

  val create : Input.t -> Diagnostics_printer.t -> t
  (** Create a lexer *)

  val next : t -> Isa_token.t Token.t * Token_info.t
  (** Returns the next token with its meta information or, if the lexer failed,
      return [None].*)

  val to_seq : t -> (Isa_token.t Token.t * Token_info.t) Sequence.t
  (** Converts a lexer into a sequence obtained by repetitive application of
      [next_token]. *)

  val to_list : t -> (Isa_token.t Token.t * Token_info.t) list
  (** Converts a lexer into a list obtained by repetitive application of
      [next_token]. *)

  val no_errors : t -> bool
end
