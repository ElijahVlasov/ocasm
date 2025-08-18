open! Import

type 't t

val create : ('t Token.t * Lexer.Token_info.t) option Sequence.t -> 't t
val next : 't t -> ('t Token.t * Lexer.Token_info.t) option
val roll_back : 't t -> unit
