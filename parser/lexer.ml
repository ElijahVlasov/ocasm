open Base

module Token = struct
  type t =
    | Colon
    | Comma
    | Decimal of string
    | End_of_file
    | End_of_line
    | ExclamaitionMark
    | LBracket
    | LCurly
    | LSquare
    | Number_label of string
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
end

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

let next : type a. a t -> char =
 fun st ->
  let module I = (val st.inp_m : Input.S with type t = a) in
  I.next st.inp

let peek : type a. a t -> char =
 fun st ->
  let module I = (val st.inp_m : Input.S with type t = a) in
  I.peek st.inp

let skip : type a. a t -> unit =
 fun st ->
  let module I = (val st.inp_m : Input.S with type t = a) in
  I.skip st.inp

let rec multiline_comment st k =
  let ch = next st in
  if ch = '*' then
    if peek st = '/' then (
      skip st;
      k st)
    else multiline_comment st k
  else if ch = Input.eof then failwith "Unfinished comment"
  else multiline_comment st k

let multiline_comment_start st k =
  skip st;
  let next_ch = next st in
  if next_ch = '*' then multiline_comment st k
  else failwith "Expected multiline comment"

let rec single_line_comment st k =
  let ch = peek st in
  match ch with
  | '\n' -> k st
  | ch when ch = Input.eof -> k st
  | _ ->
      skip st;
      single_line_comment st k

let lex_comments st ch k =
  if ch = '/' then multiline_comment_start st k
  else if ch = '#' then single_line_comment st k
  else failwith "This can't be reached"

let after_number st = Token.Decimal (Buffer.contents st.content)

let rec number st =
  let ch = peek st in
  if Char.is_digit ch then (
    Buffer.add_char st.content ch;
    skip st;
    number st)
  else after_number st

let number_start st ch =
  Buffer.clear st.content;
  if ch = '0' then
    let ch = next st in
    after_number st
  else number st

let rec skip_comments_and_whitespaces st =
  let ch = peek st in
  match ch with
  | '\t' | ' ' ->
      skip st;
      skip_comments_and_whitespaces st
  | '#' ->
      let rec ignore_until_nl st =
        let ch = peek st in
        if ch = '\n' then () else ignore_until_nl st
      in
      ignore_until_nl st
  | '/' -> multiline_comment_start st skip_comments_and_whitespaces
  | _ -> ()

let rec read_name st =
  let module Lut = Ocasm_utils.Lut in
  let ch = peek st in
  let finalize =
    (Buffer.contents st.content, Buffer.contents st.lower_case_content)
  in
  match ch with
  | ' ' | '\t' | '\n' -> finalize
  | '/' | '#' -> finalize
  | ch when ch = Input.eof -> finalize
  | ch when Lut.mem special_symbols ch -> finalize
  | _ ->
      let ch = next st in
      Buffer.add_char st.content ch;
      Buffer.add_char st.lower_case_content (Char.lowercase ch);
      read_name st

let directive_started st =
  skip st;
  Buffer.clear st.content;
  Buffer.clear st.lower_case_content;
  read_name st

let symbol_started st =
  Buffer.clear st.content;
  Buffer.clear st.lower_case_content;
  read_name st

let next_token st =
  let module Lut = Ocasm_utils.Lut in
  let open Token in
  skip_comments_and_whitespaces st;
  let ch = peek st in
  match ch with
  | '\n' -> End_of_line
  | '0' .. '9' -> number_start st ch
  | 'A' .. 'Z' | 'a' .. 'z' -> Symbol_or_opcode (symbol_started st)
  | '.' -> Symbol_or_directive (directive_started st)
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
  | ch when ch = Input.eof -> End_of_file
  | _ -> failwith "Unknown symbol"

let create inp_m inp =
  {
    inp_m;
    inp;
    content = Buffer.create 1024;
    lower_case_content = Buffer.create 1024;
  }
