type t =
  | Colon
  | Semicolon
  | Comma
  | Bin of string
  | Oct of string
  | Dec of string
  | Hex of string
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
  | Opcode of string
  | Operand of string
  | Percent
  | RBracket
  | RCurly
  | RSquare
  | Symbol of string
  | Symbol_or_directive of (string * string)
  | Symbol_or_opcode of (string * string)
  | White_space
[@@deriving eq]

val is_eof : t -> bool
val to_string : t -> string
val of_special_symbol : char -> t option
