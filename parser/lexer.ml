open Base
open Ocasm_utils

type 'a t = {
  inp_m : 'a Input.t;
  inp : 'a;
  content : Buffer.t;
  lower_case_content : Buffer.t;
}

let ( = ) = Char.equal
let whitespaces = Ocasm_utils.Lut.create [ '\n'; ' '; '\t'; Input.eof ]
let comment = Ocasm_utils.Lut.create [ '/'; '#' ]

let special_symbols =
  Ocasm_utils.Lut.create
    [ ':'; '\\'; '"'; '\''; '!'; ';'; '('; ')'; '['; ']'; '{'; '}'; Input.eof ]

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
  let ch = next st in
  if ch = '*' then
    if peek st = '/' then (
      skip st;
      k st (next st))
    else multiline_comment st k
  else if ch = Input.eof then failwith "Unfinished comment"
  else multiline_comment st k

let multiline_comment_start st k =
  let next_ch = next st in
  if next_ch = '*' then multiline_comment st k
  else failwith "Expected multiline comment"

let rec consume_while_true st pred =
  let ch = peek st in
  if pred ch then (
    Buffer.add_char st.content ch;
    skip st;
    consume_while_true st pred)
  else Buffer.contents st.content

let consume_number (type a) st bldr_m is_digit =
  let module B = (val bldr_m : Number_builder.S with type t = a) in
  let bldr = B.create () in
  let ch = ref (peek st) in
  while is_digit !ch do
    B.feed_exn bldr !ch;
    skip st;
    ch := peek st
  done;
  if Char.is_whitespace !ch || Char.is_special_symbol !ch || Char.is_eof !ch
  then B.build bldr
  else failwith "Unexpected symbol"

let bin_number st =
  let open Number_builder in
  consume_number st (module Bin_builder) Char.is_binary

let oct_number st =
  let open Number_builder in
  consume_number st (module Oct_builder) Char.is_octal

let dec_number st =
  let open Number_builder in
  consume_number st (module Dec_builder) Char.is_digit

let hex_number st =
  let open Number_builder in
  consume_number st (module Hex_builder) Char.is_hex_digit

let number_start st ch =
  let open Token in
  Buffer.clear st.content;
  Buffer.add_char st.content ch;
  if ch = '0' then
    let ch = peek st in
    if Char.is_digit ch then (
      match ch with
      | '8' | '9' -> failwith "Incorrect octal constant"
      | _ ->
          skip st;
          Buffer.add_char st.content ch;
          Oct (oct_number st))
    else if Char.lowercase ch = 'x' then (
      skip st;
      Buffer.add_char st.content ch;
      Hex (hex_number st))
    else if Char.lowercase ch = 'b' then (
      skip st;
      Buffer.add_char st.content ch;
      Bin (bin_number st))
    else Dec (Array.create ~len:1 0L)
  else Dec (dec_number st)

let skip_comments_and_whitespaces st =
  let rec skip_comments_and_whitespaces ~has_advanced st ch =
    match ch with
    | '\t' | ' ' ->
        skip st;
        skip_comments_and_whitespaces st (peek st) ~has_advanced:true
    | '#' ->
        skip st;
        let rec ignore_until_nl st =
          let ch = peek st in
          if ch = '\n' then true else ignore_until_nl st
        in
        ignore_until_nl st
    | '/' ->
        skip st;
        multiline_comment_start st
          (skip_comments_and_whitespaces ~has_advanced:true)
    | _ -> has_advanced
  in
  skip_comments_and_whitespaces ~has_advanced:false st (peek st)

let rec read_name st ch =
  let module Lut = Ocasm_utils.Lut in
  let finalize =
    (Buffer.contents st.content, Buffer.contents st.lower_case_content)
  in
  match ch with
  | ' ' | '\t' | '\n' -> finalize
  | '/' | '#' -> finalize
  | ch when ch = Input.eof -> finalize
  | ch when Lut.mem special_symbols ch -> finalize
  | _ ->
      Buffer.add_char st.content ch;
      Buffer.add_char st.lower_case_content (Char.lowercase ch);
      let ch = next st in
      read_name st ch

let directive_started st =
  skip st;
  Buffer.clear st.content;
  Buffer.clear st.lower_case_content;
  read_name st

let symbol_started st ch =
  Buffer.clear st.content;
  Buffer.clear st.lower_case_content;
  read_name st ch

let next_token st =
  let module Lut = Ocasm_utils.Lut in
  let open Token in
  let has_advanced = skip_comments_and_whitespaces st in
  if has_advanced then White_space
  else
    let ch = next st in
    match ch with
    | '\n' -> Eol
    | '0' .. '9' -> number_start st ch
    | 'A' .. 'Z' | 'a' .. 'z' -> Symbol_or_opcode (symbol_started st ch)
    | '.' -> Symbol_or_directive (directive_started st ch)
    | ',' -> Comma
    | ':' -> Colon
    | '(' -> LBracket
    | ')' -> RBracket
    | '[' -> LSquare
    | ']' -> RSquare
    | '{' -> LCurly
    | '}' -> RCurly
    | '!' -> ExclamaitionMark
    | '%' -> Percent
    | ch when ch = Input.eof -> Eof
    | _ -> failwith "Unknown symbol"

let create inp_m inp =
  {
    inp_m;
    inp;
    content = Buffer.create 1024;
    lower_case_content = Buffer.create 1024;
  }

let to_seq lexer =
  let open Base.Sequence.Generator in
  let open Token in
  let rec consume_tokens () =
    match next_token lexer with
    | Eof -> yield Eof
    | token -> yield token >>= consume_tokens
  in
  run @@ consume_tokens ()

let to_list : type a. a t -> Token.t list =
 fun lexer -> Sequence.to_list (to_seq lexer)
