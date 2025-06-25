open Base
module Bytes = Stdlib.Bytes

module type P = sig
  type instruction
  type directive

  val is_multiline_comment : int -> bool
  val is_closing_comment : int -> bool
  val is_one_line_comment : int -> bool
  val max_operands : int
  val parse_opcode : int64 -> string array -> instruction
  val parse_directive : int64 -> string list -> directive
end

module type S = sig
  include Language.E

  val parse : bytes -> expr list
end

module Make (P : P) : S with type instruction := P.instruction
