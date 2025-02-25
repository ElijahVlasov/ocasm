open Lexing

type lexing_error = InvalidCharacter of string [@@deriving show, eq]

exception LexError of lexing_error

let equal_position = ( = )

let pp_position fmt pos =
  Format.fprintf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

type parsing_error =
  | InvalidRegister of string
  | InvalidImmediate of int32
  | InvalidLabel of string
  | InvalidInstruction of string
  | InvalidSyntax of (position * position)
[@@deriving show, eq]

type parser_error =
  | LexingError of lexing_error
  | ParsingError of parsing_error
[@@deriving show, eq]

type 'a parser_result = ('a, parser_error) result
