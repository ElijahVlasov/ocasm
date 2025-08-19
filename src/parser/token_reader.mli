open! Import

type 't t

exception Lexer_error

val create : ('t Token.t * Lexer.Token_info.t) option Sequence.t -> 't t
val next : 't t -> 't Token.t
val peek : 't t -> 't Token.t
val last_token_info : 't t -> Lexer.Token_info.t
