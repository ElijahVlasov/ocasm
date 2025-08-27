open! Import

type 'a t =
  | Colon
  | Semicolon
  | Comma
  | Bin of Big_integer.t
  | Oct of Big_integer.t
  | Dec of Big_integer.t
  | Hex of Big_integer.t
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
  | String_literal of string
  | Percent
  | RBracket
  | RCurly
  | RSquare
  | Symbol of string
  | White_space
  | Isa_specific of 'a
[@@deriving eq, show]

val is_eof : 'a t -> bool
val is_whitespace : 'a t -> bool
val is_eol : 'a t -> bool

module MkToken (Isa_specific : sig
  type t

  include To_string.S with type t := t
  include Equal.S with type t := t
  include Pretty_printer.S with type t := t
end) : sig
  type t := Isa_specific.t t

  include To_string.S with type t := t
  include Equal.S with type t := t
  include Pretty_printer.S with type t := t
end

val of_special_symbol : char -> 'a t option
