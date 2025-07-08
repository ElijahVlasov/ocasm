open Base

module Token : sig
  type t =
    | Colon
    | Comma
    | Bin of string
    | Oct of string
    | Dec of string
    | Hex of string
    | End_of_file
    | End_of_line
    | ExclamaitionMark
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
  [@@deriving eq, show]

  val is_eof : t -> bool
end

type 'a t

val create : 'a Input.t -> 'a -> 'a t
val next_token : 'a t -> Token.t
val to_seq : 'a t -> Token.t Sequence.t
val to_list : 'a t -> Token.t list
