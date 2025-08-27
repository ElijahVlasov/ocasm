open! Import

exception WrongDigit of char

module type D = sig
  val bit_len : int
  val get_digit : char -> int64
  val is_digit : char -> bool
  val prefix : string
end

module type S = sig
  type t

  val is_digit : char -> bool
  val create : unit -> t
  val clear : t -> unit
  val add_char : t -> char -> unit
  val build : t -> Big_integer.t
end

module MkBuilder (D : D) = struct
  include D

  type t = { mutable digits : char list; mutable len : int }

  let create () = { digits = []; len = 0 }
  let clear b = b.digits <- []

  let add_char st ch =
    st.len <- st.len + 1;
    st.digits <- List.cons ch st.digits

  let build st =
    if st.len = 0 then Big_integer.zero
    else
      let buf = Buffer.create st.len in
      Buffer.add_string buf D.prefix;
      List.iter ~f:(fun x -> Buffer.add_char buf x) (List.rev st.digits);
      Big_integer.of_buffer buf
end

module Dec_builder : S = struct
  type t = unit

  let create () = ()
  let clear st = Panic.unimplemented ()
  let add_char st ch = Panic.unimplemented ()
  let build st = Panic.unimplemented ()
  let is_digit = Char.is_digit
end

module Bin_builder : S = MkBuilder (struct
  let bit_len = 1
  let get_digit = function '0' -> 0L | '1' -> 1L | ch -> raise (WrongDigit ch)
  let is_digit = Char.is_binary
  let prefix = "0b"
end)

module Oct_builder : S = MkBuilder (struct
  let bit_len = 3
  let is_digit = Char.is_octal

  let get_digit = function
    | '0' -> 0L
    | '1' -> 1L
    | '2' -> 2L
    | '3' -> 3L
    | '4' -> 4L
    | '5' -> 5L
    | '6' -> 6L
    | '7' -> 7L
    | ch -> raise (WrongDigit ch)

  let prefix = "0o"
end)

module Hex_builder : S = MkBuilder (struct
  let bit_len = 4
  let is_digit = Char.is_hex_digit

  let get_digit = function
    | '0' -> 0L
    | '1' -> 1L
    | '2' -> 2L
    | '3' -> 3L
    | '4' -> 4L
    | '5' -> 5L
    | '6' -> 6L
    | '7' -> 7L
    | '8' -> 8L
    | '9' -> 9L
    | 'a' -> 10L
    | 'b' -> 11L
    | 'c' -> 12L
    | 'd' -> 13L
    | 'e' -> 14L
    | 'f' -> 15L
    | ch -> raise (WrongDigit ch)

  let prefix = "0x"
end)
