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

module Recovery = struct
  type t = No_recovery | Read_to_eol | Custom of (unit -> unit)

  let to_func st = function
    | No_recovery -> None
    | Read_to_eol -> Some (fun () -> Lexer_state.consume_until_nl st)
    | Custom k -> Some k
end

module Error = struct
  type t =
    | Incorrect_escape_sequence of char
    | Incorrect_hex_escape_sequence of char * char
    | Unfinished_comment
    | Unfinished_string_literal
    | Expected_vs_got of char * char
    | Wrong_char_in_number_literal of char
    | Junk_symbol of char

  let msg = function
    | Incorrect_escape_sequence c ->
        Printf.sprintf "incorrect escape sequence '\\%c'" c
    | Incorrect_hex_escape_sequence (fst, snd) ->
        Printf.sprintf "incorrect hex literal in an escape sequence '\\x%c%c'"
          fst snd
    | Unfinished_comment -> "unfinished multiline comment, EOF was reached"
    | Unfinished_string_literal -> "unfinished string literal, EOF was reached"
    | Expected_vs_got (expected, got) ->
        Printf.sprintf "'%c' was expected but got '%c'" expected got
    | Wrong_char_in_number_literal c ->
        Printf.sprintf "unexpected character '%c' in a number literal" c
    | Junk_symbol c -> Printf.sprintf "junk symbol '%c'" c

  let id = function
    | Incorrect_escape_sequence _ -> 0
    | Incorrect_hex_escape_sequence (_, _) -> 1
    | Unfinished_comment -> 2
    | Unfinished_string_literal -> 6
    | Expected_vs_got (_, _) -> 3
    | Wrong_char_in_number_literal _ -> 4
    | Junk_symbol _ -> 5
end

module Warning = struct
  type t = Newline_in_string_literal

  let msg = function
    | Newline_in_string_literal -> "newline character in a string literal"

  let id = function Newline_in_string_literal -> 0
end

type ('a, 't) t = {
  diagnostics : Diagnostics.t;
  isa_m : (module Isa_token.S with type t = 't);
  lexer_state : 'a Lexer_state.t;
  mutable recovery : (unit -> unit) option;
}

let error_aux recovery st err =
  let open Diagnostics in
  error
    ?recovery:(Recovery.to_func st.lexer_state recovery)
    st.diagnostics
    {
      msg = Error.msg err;
      id = Error.id err;
      left = (0, 0);
      right = (0, 0);
      file = Path.of_string "";
      ctx = "";
    }

let warn_aux recovery st w : 'b =
  let open Diagnostics in
  warn
    ?recovery:(Recovery.to_func st.lexer_state recovery)
    st.diagnostics
    {
      msg = Warning.msg w;
      id = Warning.id w;
      left = (0, 0);
      right = (0, 0);
      file = Path.of_string "";
      ctx = "";
    }

let error ?k = Option.value ~default:Recovery.Read_to_eol k |> error_aux
let warn ?k = Option.value ~default:Recovery.Read_to_eol k |> warn_aux
let fail st err = error_aux Recovery.No_recovery st err

open Warning
open Error

let rec multiline_comment st k =
  let open Char in
  let ch = next st.lexer_state in
  if ch = '*' then
    if peek st.lexer_state = '/' then (
      skip st.lexer_state;
      k st (next st.lexer_state))
    else multiline_comment st k
  else if Char.is_eof ch then error st Unfinished_comment
  else multiline_comment st k

let multiline_comment_start st k =
  let open Char in
  let next_ch = next st.lexer_state in
  if next_ch = '*' then multiline_comment st k
  else error st @@ Expected_vs_got ('*', next_ch)

let consume_number st is_digit buf =
  add_to_buf_while_true st.lexer_state buf is_digit;
  let ch = peek st.lexer_state in
  if Char.is_word_separator ch then Buffer.to_number buf
  else error st @@ Wrong_char_in_number_literal ch

let number_start st ch =
  let open Char in
  let open Number_builder in
  if ch = '0' then
    match Char.lowercase (peek st.lexer_state) with
    | 'x' ->
        skip st.lexer_state;
        with_number_builder st.lexer_state Hex @@ fun buf ->
        Token.Hex (consume_number st Hex_builder.is_digit buf)
    | 'b' ->
        skip st.lexer_state;
        with_number_builder st.lexer_state Bin @@ fun buf ->
        Token.Bin (consume_number st Bin_builder.is_digit buf)
    | ch when Char.is_octal ch ->
        (* Note we shouldn't skip here as the next char *)
        (* is a part of the number in question. *)
        with_number_builder st.lexer_state Oct @@ fun buf ->
        Token.Oct (consume_number st Oct_builder.is_digit buf)
    | ch when Char.is_word_separator ch -> Dec (Array.create ~len:1 0L)
    | ch -> error st @@ Wrong_char_in_number_literal ch
  else
    with_number_builder st.lexer_state Dec @@ fun buf ->
    Buffer.add_char buf ch;
    Token.Dec (consume_number st Dec_builder.is_digit buf)

let skip_comments_and_whitespaces st =
  let rec skip_comments_and_whitespaces ~has_advanced st ch =
    match ch with
    | '\t' | ' ' | '\r' ->
        skip st.lexer_state;
        skip_comments_and_whitespaces st (peek st.lexer_state)
          ~has_advanced:true
    | '#' ->
        skip st.lexer_state;
        consume_until_nl st.lexer_state;
        true
    | '/' ->
        skip st.lexer_state;
        multiline_comment_start st
          (skip_comments_and_whitespaces ~has_advanced:true)
    | _ -> has_advanced
  in
  skip_comments_and_whitespaces ~has_advanced:false st (peek st.lexer_state)

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

let read_escaped k st buf =
  Buffer.add_char buf
  @@
  match next st.lexer_state with
  | 'a' -> '\x07'
  | 'b' -> '\x08'
  | 't' -> '\t'
  | 'n' -> '\n'
  | 'v' -> '\x0B'
  | 'f' -> '\x0C'
  | 'r' -> '\r'
  | 'x' ->
      let fst = next st.lexer_state in
      let snd = next st.lexer_state in
      Option.value_or_thunk ~default:(fun () ->
          error ~k st @@ Incorrect_hex_escape_sequence (fst, snd))
      @@ Char.of_hex_digits_le fst snd
  | '"' -> '"'
  | ch -> error ~k st @@ Incorrect_escape_sequence ch

let rec read_string_literal st buf =
  let k = Recovery.Custom (fun () -> Fn.ignore @@ read_string_literal st buf) in
  match next st.lexer_state with
  | '"' -> String_literal (Buffer.contents buf)
  | '\\' ->
      read_escaped k st buf;
      read_string_literal st buf
  | '\n' ->
      warn ~k st Newline_in_string_literal;
      Buffer.add_char buf '\n';
      read_string_literal st buf
  | '\x00' -> fail st Unfinished_string_literal
  | ch ->
      Buffer.add_char buf ch;
      read_string_literal st buf

let string_literal_started st =
  with_case_sensitive_buf st.lexer_state @@ read_string_literal st

let recover st =
  match st.recovery with
  | None -> ()
  | Some recovery ->
      st.recovery <- None;
      recovery ()

let next_token_aux (type t) st =
  let module T = (val st.isa_m : Isa_token.S with type t = t) in
  try
    recover st;
    let has_advanced = skip_comments_and_whitespaces st in
    Option.some
    @@
    if has_advanced then White_space
    else
      let ch = next st.lexer_state in
      match ch with
      | '\n' -> Eol
      | '0' .. '9' -> number_start st ch
      | 'A' .. 'Z' | 'a' .. 'z' | '_' ->
          name_like_started ~isa_specific:T.name st.isa_m st.lexer_state ch
      | '.' ->
          name_like_started ~isa_specific:T.directive ~default:Dot st.isa_m
            st.lexer_state ch
      | '%' ->
          name_like_started ~isa_specific:T.reserved ~default:Percent st.isa_m
            st.lexer_state ch
      | '"' -> string_literal_started st
      | '\x00' -> Eof
      | ch when Char.is_nonascii ch ->
          with_case_sensitive_buf st.lexer_state @@ fun buf ->
          Buffer.add_char buf ch;
          read_proper_name st.lexer_state buf
      | ch ->
          Option.value_or_thunk
            ~default:(fun () -> error st @@ Junk_symbol ch)
            (Token.of_special_symbol ch)
  with
  | Diagnostics.Recoverable k ->
      st.recovery <- Some k;
      None
  | Diagnostics.Non_recoverable -> None
  | e -> raise e

let next_token st =
  let token_opt = ref (next_token_aux st) in
  while Option.is_none !token_opt do
    token_opt := next_token_aux st
  done;
  Option.value_exn !token_opt

let create ?diagnostics isa_m inp_m inp =
  let diagnostics =
    Option.value_or_thunk diagnostics ~default:Diagnostics.create
  in
  {
    diagnostics;
    isa_m;
    lexer_state = Lexer_state.create inp_m inp;
    recovery = None;
  }

let to_seq lexer =
  let open Base.Sequence.Generator in
  let rec consume_tokens () =
    match next_token lexer with
    | Eof -> yield @@ Eof
    | token -> yield token >>= consume_tokens
  in
  run @@ consume_tokens ()

let to_list lexer = Sequence.to_list (to_seq lexer)
