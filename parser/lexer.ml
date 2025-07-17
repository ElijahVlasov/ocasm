open Base
open Ocasm_utils

module type T = sig
  type token

  val directive : string -> token option
  val name : string -> token option
  val reserved : string -> token option
end

module type B = sig
  type t

  val add_char : t -> char -> unit
  val clear : t -> unit
end

module Lc_buffer : sig
  include B

  val create : int -> t
  val content : t -> string
  val lc_content : t -> string
  val to_buffer : t -> Buffer.t
end = struct
  type t = { content : Buffer.t; lc_content : Buffer.t }

  let create n = { content = Buffer.create n; lc_content = Buffer.create n }

  let add_char b c =
    Buffer.add_char b.content c;
    Buffer.add_char b.lc_content (Char.lowercase c)

  let clear b =
    Buffer.clear b.content;
    Buffer.clear b.lc_content

  let content b = Buffer.contents b.content
  let lc_content b = Buffer.contents b.lc_content
  let to_buffer b = b.content
end

type ('a, 't) t = {
  isa_token_m : (module T with type token = 't);
  inp_m : 'a Input.t;
  inp : 'a;
  token_buf : Lc_buffer.t;
}

let next (type a) st =
  let module I = (val st.inp_m : Input.S with type t = a) in
  I.next st.inp

let peek (type a) st =
  let module I = (val st.inp_m : Input.S with type t = a) in
  I.peek st.inp

let skip (type a) st =
  let module I = (val st.inp_m : Input.S with type t = a) in
  I.skip st.inp

let rec multiline_comment st k =
  let open Char in
  let ch = next st in
  if ch = '*' then
    if peek st = '/' then (
      skip st;
      k st (next st))
    else multiline_comment st k
  else if ch = Input.eof then failwith "Unfinished comment"
  else multiline_comment st k

let multiline_comment_start st k =
  let open Char in
  let next_ch = next st in
  if next_ch = '*' then multiline_comment st k
  else failwith "Expected multiline comment"

let consume_while_true (type a) bldr_m bldr st pred =
  let module B = (val bldr_m : B with type t = a) in
  let ch = ref (peek st) in
  while pred !ch do
    skip st;
    B.add_char bldr !ch;
    ch := peek st
  done

let consume_until_nl st =
  consume_while_true
    (module struct
      type t = unit

      let add_char _ _ = ()
      let clear _ = ()
    end)
    () st
    (fun ch -> not @@ Char.is_newline ch)

let consume_number (type a) st bldr_m =
  let module B = (val bldr_m : Number_builder.S with type t = a) in
  let bldr = B.create () in
  consume_while_true (module B) bldr st B.is_digit;
  let ch = peek st in
  if Char.is_word_separator ch then B.build bldr
  else failwith "Unexpected symbol"

let bin_number st =
  let open Number_builder in
  consume_number st (module Bin_builder)

let oct_number st =
  let open Number_builder in
  consume_number st (module Oct_builder)

let dec_number st =
  let open Number_builder in
  consume_number st (module Dec_builder)

let hex_number st =
  let open Number_builder in
  consume_number st (module Hex_builder)

let number_start st ch =
  let open Char in
  let open Token in
  let token_buf = Lc_buffer.to_buffer st.token_buf in
  Buffer.add_char token_buf ch;
  if ch = '0' then
    let ch = peek st in
    match Char.lowercase ch with
    | 'x' ->
        skip st;
        Hex (hex_number st)
    | 'b' ->
        skip st;
        Bin (bin_number st)
    | ch when Char.is_octal ch ->
        skip st;
        Oct (oct_number st)
    | ch when Char.is_word_separator ch -> Dec (Array.create ~len:1 0L)
    | _ -> failwith "Unexpected char"
  else Dec (dec_number st)

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

let read_proper_name st token_buf =
  consume_while_true
    (module Buffer)
    token_buf st
    (fun ch -> Char.is_valid_name_symbol ch || Char.is_nonascii ch);
  Token.Name (Buffer.contents token_buf)

let read_name st k =
  consume_while_true
    (module Lc_buffer)
    st.token_buf st Char.is_valid_name_symbol;
  let ch = ref (peek st) in
  if Char.is_nonascii !ch then
    read_proper_name st (Lc_buffer.to_buffer st.token_buf)
  else k (Lc_buffer.content st.token_buf) (Lc_buffer.lc_content st.token_buf)

let name_like_started (type t) ~isa_specific ?default st ch =
  let module T = (val st.isa_token_m : T with type token = t) in
  Lc_buffer.clear st.token_buf;
  Lc_buffer.add_char st.token_buf ch;
  read_name st @@ fun name lc_name ->
  if String.length lc_name <> 1 || Option.is_none default then
    match isa_specific lc_name with
    | None -> Token.Name name
    | Some t -> Token.Isa_specific t
  else Option.value_exn default

let next_token (type t) st =
  let module T = (val st.isa_token_m : T with type token = t) in
  let open Token in
  let has_advanced = skip_comments_and_whitespaces st in
  if has_advanced then White_space
  else
    let ch = next st in
    match ch with
    | '\n' -> Eol
    | '0' .. '9' -> number_start st ch
    | 'A' .. 'Z' | 'a' .. 'z' -> name_like_started ~isa_specific:T.name st ch
    | '.' -> name_like_started ~isa_specific:T.directive ~default:Dot st ch
    | '%' -> name_like_started ~isa_specific:T.reserved ~default:Percent st ch
    | ',' -> Comma
    | ':' -> Colon
    | '(' -> LBracket
    | ')' -> RBracket
    | '[' -> LSquare
    | ']' -> RSquare
    | '{' -> LCurly
    | '}' -> RCurly
    | '!' -> ExclamaitionMark
    | '\x00' -> Eof
    | ch when Char.is_nonascii ch ->
        let token_buf = Lc_buffer.to_buffer st.token_buf in
        Buffer.clear token_buf;
        Buffer.add_char token_buf ch;
        read_proper_name st token_buf
    | _ -> failwith "Unknown symbol"

let create isa_token_m inp_m inp =
  { isa_token_m; inp_m; inp; token_buf = Lc_buffer.create 1024 }

let to_seq lexer =
  let open Base.Sequence.Generator in
  let open Token in
  let rec consume_tokens () =
    match next_token lexer with
    | Eof -> yield @@ Eof
    | token -> yield token >>= consume_tokens
  in
  run @@ consume_tokens ()

let to_list lexer = Sequence.to_list (to_seq lexer)
