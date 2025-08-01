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

module MkToken (Isa_specific : sig
  type t

  include To_string.S with type t := t
  include Equal.S with type t := t
end) : sig
  type t := Isa_specific.t t

  include To_string.S with type t := t
  include Equal.S with type t := t
end = struct
  let to_string = function
    | Colon -> ":"
    | Semicolon -> ";"
    | Comma -> ","
    | Bin x | Oct x | Dec x | Hex x ->
        (* TODO : implement this *)
        if Array.length x = 1 then Int64.to_string (Array.get x 0) else ""
    | Eof -> "EOF"
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
    | Name name -> name
    | Opcode x -> x
    | Operand x -> x
    | String_literal x -> Printf.sprintf "%S" x
    | Percent -> "%"
    | RBracket -> ")"
    | RCurly -> "}"
    | RSquare -> "]"
    | Symbol x -> x
    | Symbol_or_directive (x, _) -> x
    | Symbol_or_opcode (x, _) -> x
    | White_space -> " "
    | Isa_specific t -> Isa_specific.to_string t

  let equal x y = equal Isa_specific.equal x y
end

let is_eof = function Eof -> true | _ -> false

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
