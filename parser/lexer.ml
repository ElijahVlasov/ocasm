open Base
open Ocasm_utils
open Token
open Lexer_state

module Isa_token = struct
  module type S = sig
    type t

    val directive : string -> t option
    val name : string -> t option
    val reserved : string -> t option
  end
end

exception IncorrectEscapeSequence of char
exception IncorrectHexEscapeSequence of char * char
exception UnfinishedComment
exception ExpectedGot of char * char
exception WrongCharInNumberLiteral of char
exception JunkSymbol of char

type ('a, 't) t = {
  isa_m : (module Isa_token.S with type t = 't);
  st : 'a Lexer_state.t;
}

let rec multiline_comment st k =
  let open Char in
  let ch = next st in
  if ch = '*' then
    if peek st = '/' then (
      skip st;
      k st (next st))
    else multiline_comment st k
  else if Char.is_eof ch then raise UnfinishedComment
  else multiline_comment st k

let multiline_comment_start st k =
  let open Char in
  let next_ch = next st in
  if next_ch = '*' then multiline_comment st k
  else raise @@ ExpectedGot ('*', next_ch)

let consume_number st is_digit buf =
  add_to_buf_while_true st buf is_digit;
  let ch = peek st in
  if Char.is_word_separator ch then Buffer.to_number buf
  else raise @@ WrongCharInNumberLiteral ch

let number_start st ch =
  let open Char in
  let open Number_builder in
  if ch = '0' then
    match Char.lowercase (peek st) with
    | 'x' ->
        skip st;
        with_number_builder st Hex @@ fun buf ->
        Token.Hex (consume_number st Hex_builder.is_digit buf)
    | 'b' ->
        skip st;
        with_number_builder st Bin @@ fun buf ->
        Token.Bin (consume_number st Bin_builder.is_digit buf)
    | ch when Char.is_octal ch ->
        (* Note we shouldn't skip here as the next char *)
        (* is a part of the number in question. *)
        with_number_builder st Oct @@ fun buf ->
        Token.Oct (consume_number st Oct_builder.is_digit buf)
    | ch when Char.is_word_separator ch -> Dec (Array.create ~len:1 0L)
    | ch -> raise @@ WrongCharInNumberLiteral ch
  else
    with_number_builder st Dec @@ fun buf ->
    Buffer.add_char buf ch;
    Token.Dec (consume_number st Dec_builder.is_digit buf)

let skip_comments_and_whitespaces st =
  let rec skip_comments_and_whitespaces ~has_advanced st ch =
    match ch with
    | '\t' | ' ' | '\r' ->
        skip st;
        skip_comments_and_whitespaces st (peek st) ~has_advanced:true
    | '#' ->
        skip st;
        consume_until_nl st;
        true
    | '/' ->
        skip st;
        multiline_comment_start st
          (skip_comments_and_whitespaces ~has_advanced:true)
    | _ -> has_advanced
  in
  skip_comments_and_whitespaces ~has_advanced:false st (peek st)

let read_proper_name st (buf : case_sensitive Buffer.t) =
  add_to_buf_while_true st buf (fun ch ->
      Char.is_valid_name_symbol ch || Char.is_nonascii ch);
  Name (Buffer.contents buf)

let read_name st buf k =
  add_to_buf_while_true st buf Char.is_valid_name_symbol;
  let ch = ref (peek st) in
  if Char.is_nonascii !ch then
    continue_case_sensitive_buf st @@ read_proper_name st
  else (* TODO: what if we return thunks here instead of strings *)
    k (Buffer.contents buf) (Buffer.lc_contents buf)

let name_like_started (type t) ~isa_specific ?default isa_m st ch =
  let module T = (val isa_m : Isa_token.S with type t = t) in
  with_case_insensitive_buf st @@ fun buf ->
  Buffer.add_char buf ch;
  read_name st buf @@ fun name lc_name ->
  if String.length lc_name <> 1 || Option.is_none default then
    match isa_specific lc_name with
    | None -> Name name
    | Some t -> Isa_specific t
  else Option.value_exn default

let read_escaped st buf =
  Buffer.add_char buf
  @@
  match next st with
  | 'a' -> '\x07'
  | 'b' -> '\x08'
  | 't' -> '\t'
  | 'n' -> '\n'
  | 'v' -> '\x0B'
  | 'f' -> '\x0C'
  | 'r' -> '\r'
  | 'x' ->
      let fst = next st in
      let snd = next st in
      Option.value_or_thunk ~default:(fun () ->
          raise @@ IncorrectHexEscapeSequence (fst, snd))
      @@ Char.of_hex_digits_le fst snd
  | '"' -> '"'
  | ch -> raise @@ IncorrectEscapeSequence ch

let rec read_string_literal st buf =
  match next st with
  | '"' -> String_literal (Buffer.contents buf)
  | '\\' ->
      read_escaped st buf;
      read_string_literal st buf
  | '\n' -> failwith "Newline in a string literal"
  | ch ->
      Buffer.add_char buf ch;
      read_string_literal st buf

let string_literal_started st =
  with_case_sensitive_buf st @@ read_string_literal st

let next_token (type t) lexer =
  let module T = (val lexer.isa_m : Isa_token.S with type t = t) in
  let st = lexer.st in
  let has_advanced = skip_comments_and_whitespaces st in
  if has_advanced then White_space
  else
    let ch = next st in
    match ch with
    | '\n' -> Eol
    | '0' .. '9' -> number_start st ch
    | 'A' .. 'Z' | 'a' .. 'z' | '_' ->
        name_like_started ~isa_specific:T.name lexer.isa_m st ch
    | '.' ->
        name_like_started ~isa_specific:T.directive ~default:Dot lexer.isa_m st
          ch
    | '%' ->
        name_like_started ~isa_specific:T.reserved ~default:Percent lexer.isa_m
          st ch
    | '"' -> string_literal_started st
    | '\x00' -> Eof
    | ch when Char.is_nonascii ch ->
        with_case_sensitive_buf st @@ fun buf ->
        Buffer.add_char buf ch;
        read_proper_name st buf
    | ch ->
        Option.value_or_thunk
          ~default:(fun () -> raise @@ JunkSymbol ch)
          (Token.of_special_symbol ch)

let create isa_m inp_m inp = { isa_m; st = Lexer_state.create inp_m inp }

let to_seq lexer =
  let open Base.Sequence.Generator in
  let rec consume_tokens () =
    match next_token lexer with
    | Eof -> yield @@ Eof
    | token -> yield token >>= consume_tokens
  in
  run @@ consume_tokens ()

let to_list lexer = Sequence.to_list (to_seq lexer)
