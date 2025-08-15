open! Import

type 'a t =
  | Colon
  | Semicolon
  | Comma
  | Bin of int64 array
  | Oct of int64 array
  | Dec of int64 array
  | Hex of int64 array
  | Eof
  | Eol
  | ExclamaitionMark
  | Hash
  | Dollar
  | Amp
  | Mul
  | Plus
  | Eq
  | Lt
  | Gt
  | QuestionMark
  | At
  | Dot
  | Slash
  | Bslash
  | Caret
  | Btick
  | Vbar
  | Tilde
  | LBracket
  | LCurly
  | LSquare
  | Name of string
  | Opcode of string
  | Operand of string
  | String_literal of string
  | Percent
  | RBracket
  | RCurly
  | RSquare
  | Symbol of string
  | Symbol_or_directive of (string * string)
  | Symbol_or_opcode of (string * string)
  | White_space
  | Isa_specific of 'a
[@@deriving eq]

val is_eof : 'a t -> bool
val is_whitespace : 'a t -> bool

module MkToken (Isa_specific : sig
  type t

  include To_string.S with type t := t
  include Equal.S with type t := t
end) : sig
  type t := Isa_specific.t t

  include To_string.S with type t := t
  include Equal.S with type t := t
end

val is_eof : 'a t -> bool
val of_special_symbol : char -> 'a t option
