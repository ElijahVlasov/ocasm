open! Import

type 't t

val create : ('t Token.t * Lexer.Token_info.t) Sequence.t -> 't t
val next : 't t -> 't Token.t
val peek : 't t -> 't Token.t
val last_token_info : 't t -> Lexer.Token_info.t
