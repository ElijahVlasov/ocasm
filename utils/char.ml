open Base
include Base.Char

let is_binary = function '0' | '1' -> true | _ -> false
let is_octal = function '0' .. '7' -> true | _ -> false
let is_newline = function '\n' -> true | _ -> false

let is_special_symbol = function
  | '.' | '@' | ':' | ';' | ',' | '!' | '#' | '$' | '&' | '*' | '+' | '=' | '<'
  | '>' | '?' | '/' | '\\' | '^' | '`' | '|' | '~' | '(' | '{' | '[' | '%' | ')'
  | '}' | ']' ->
      true
  | _ -> false

let is_eof = function '\x00' -> true | _ -> false
let is_nonascii ch = '\x80' <= ch

let is_valid_name_symbol = function
  | 'A' .. 'Z' | 'a' .. 'z' | '.' | '_' | '0' .. '9' -> true
  | _ -> false

let is_word_separator ch =
  if ch = '.' then false
  else is_special_symbol ch || is_eof ch || is_whitespace ch

let of_hex_digits_le fst snd =
  let fst = get_hex_digit fst in
  let snd = get_hex_digit snd in
  match (fst, snd) with
  | Some fst, Some snd -> Some (Char.of_int_exn (Int.shift_left fst 4 lor snd))
  | _ -> None
