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

let is_eof = function Eof -> true | _ -> false

let to_string = function
  | Colon -> ":"
  | Semicolon -> ";"
  | Comma -> ","
  | Bin x | Oct x | Dec x | Hex x -> x
  | Eof -> "\\x00"
  | Eol -> "\\n"
  | ExclamaitionMark -> "!"
  | Hash -> "#"
  | Dollar -> "$"
  | Amp -> "&"
  | Mul -> "*"
  | Plus -> "+"
  | Eq -> "="
  | Lt -> "<"
  | Gt -> ">"
  | QuestionMark -> "?"
  | At -> "@"
  | Dot -> "."
  | Slash -> "/"
  | Bslash -> "\\"
  | Caret -> "^"
  | Btick -> "`"
  | Vbar -> "|"
  | Tilde -> "~"
  | LBracket -> "("
  | LCurly -> "{"
  | LSquare -> "["
  | Opcode x -> x
  | Operand x -> x
  | Percent -> "%"
  | RBracket -> ")"
  | RCurly -> "}"
  | RSquare -> "]"
  | Symbol x -> x
  | Symbol_or_directive (x, _) -> x
  | Symbol_or_opcode (x, _) -> x
  | White_space -> " "

let of_special_symbol = function
  | '.' -> Some Dot
  | '@' -> Some At
  | ':' -> Some Colon
  | ';' -> Some Semicolon
  | ',' -> Some Comma
  | '!' -> Some ExclamaitionMark
  | '#' -> Some Hash
  | '$' -> Some Dollar
  | '&' -> Some Amp
  | '*' -> Some Mul
  | '+' -> Some Plus
  | '=' -> Some Eq
  | '<' -> Some Lt
  | '>' -> Some Gt
  | '?' -> Some QuestionMark
  | '/' -> Some Slash
  | '\\' -> Some Bslash
  | '^' -> Some Caret
  | '`' -> Some Btick
  | '|' -> Some Vbar
  | '~' -> Some Tilde
  | '(' -> Some LBracket
  | '{' -> Some LCurly
  | '[' -> Some LSquare
  | '%' -> Some Percent
  | ')' -> Some RBracket
  | '}' -> Some RCurly
  | ']' -> Some RSquare
  | _ -> None
