open Base
open Ocasm_utils

exception WrongDigit of char

module type D = sig
  val bit_len : int
  val get_digit : char -> int64
  val is_digit : char -> bool
end

module type S = sig
  type t

  val is_digit : char -> bool
  val create : unit -> t
  val clear : t -> unit
  val add_char : t -> char -> unit
  val build : t -> int64 array
end

module MkBuilder (D : D) = struct
  include D

  type t = {
    mutable hd : int64;
    mutable limbs : int64 list;
    mutable hd_bit_len : int;
    mutable limbs_len : int;
  }

  let create () = { hd = 0L; limbs = []; hd_bit_len = 0; limbs_len = 0 }

  let clear b =
    b.hd <- 0L;
    b.limbs <- [];
    b.hd_bit_len <- 0;
    b.limbs_len <- 0

  let add_hd st =
    st.limbs <- List.cons st.hd st.limbs;
    st.limbs_len <- st.limbs_len + 1

  open Int64

  let ( = ) = Int.( = )
  let ( < ) = Int.( < )
  let ( + ) = Int.( + )
  let ( - ) = Int.( - )

  let add_char st ch =
    let d = get_digit ch in
    if st.hd_bit_len < 64 then (
      st.hd <-
        (let hd_shifted = shift_left st.hd bit_len in
         hd_shifted lor d);
      st.hd_bit_len <- st.hd_bit_len + bit_len)
    else (
      add_hd st;
      st.hd_bit_len <- bit_len;
      st.hd <- d)

  let consume_shift lst arr diff =
    if diff = 0 then Array.fill_from_list arr lst
    else
      let diff_comp = 64 - diff in
      let mask = Masks.first_n_bits diff in
      let rec consume_aux lst arr diff i =
        match lst with
        | [] -> ()
        | limb :: [] -> Array.set arr i (shift_right_logical limb diff)
        | limb :: (next :: rst as tail) ->
            let shifted = shift_right_logical limb diff in
            let next_diff = next land mask in
            let next_diff = shift_left next_diff diff_comp in
            Array.set arr i (shifted lor next_diff);
            consume_aux tail arr diff (Int.succ i)
      in
      consume_aux lst arr diff 0

  let build st =
    let res =
      let len = st.limbs_len + 1 in
      Array.create ~len 0L
    in
    let diff = 64 - st.hd_bit_len in
    consume_shift (shift_left st.hd diff :: st.limbs) res diff;
    res
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
end)
