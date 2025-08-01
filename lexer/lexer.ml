open Import

module Isa_token = struct
  module type S = sig
    type t

    val directive : string -> t option
    val name : string -> t option
    val reserved : string -> t option

    include To_string.S with type t := t
    include Equal.S with type t := t
  end
end

type ('a, 'h, 't) t = {
  isa_m : (module Isa_token.S with type t = 't);
  state : ('a, 'h) Lexer_state.t;
  mutable recovery : (unit -> unit) option;
}

module Token_info = struct
  type t = {
    starts : Location.t;
    ends : Location.t;
    string : unit -> string;
        [@equal fun x y -> String.equal (x ()) (y ())]
        [@printer fun fmt x -> fprintf fmt "%s" (x ())]
  }
  [@@deriving eq, show]

  let to_string = show
end

let return_info_simple (type t) st token =
  let module T = Token.MkToken ((val st.isa_m : Isa_token.S with type t = t)) in
  let open Token_info in
  ( token,
    {
      starts = Lexer_state.get_start st.state;
      ends = Lexer_state.pos st.state;
      string = (fun () -> T.to_string token);
    } )

let return_info_thunk st token_thunk token =
  let open Token_info in
  ( token,
    {
      starts = Lexer_state.get_start st.state;
      ends = Lexer_state.pos st.state;
      string = token_thunk;
    } )

let rec multiline_comment st k =
  let open Char in
  let ch = Lexer_state.next st.state in
  if ch = '*' then
    if Lexer_state.peek st.state = '/' then (
      Lexer_state.skip st.state;
      k st (Lexer_state.next st.state))
    else multiline_comment st k
  else if is_eof ch then Lexer_state.error st.state Unfinished_comment
  else multiline_comment st k

let multiline_comment_start st k =
  let open Char in
  let next_ch = Lexer_state.next st.state in
  if next_ch = '*' then multiline_comment st k
  else Lexer_state.error st.state @@ Expected_vs_got ('*', next_ch)

let consume_number st is_digit builder =
  Lexer_state.add_to_builder_while_true st.state builder is_digit;
  let ch = Lexer_state.peek st.state in
  if Char.is_word_separator ch then Token_builder.to_number builder
  else Lexer_state.error st.state @@ Wrong_char_in_number_literal ch

let number_start st ch =
  let open Char in
  let open Number_builder in
  if ch = '0' then
    match lowercase (Lexer_state.peek st.state) with
    | 'x' ->
        Lexer_state.skip st.state;
        Lexer_state.with_number_builder st.state Hex @@ fun builder ->
        Token.Hex (consume_number st Hex_builder.is_digit builder)
        |> return_info_thunk st (fun () -> Token_builder.contents builder)
    | 'b' ->
        Lexer_state.skip st.state;
        Lexer_state.with_number_builder st.state Bin @@ fun builder ->
        Token.Bin (consume_number st Bin_builder.is_digit builder)
        |> return_info_thunk st (fun () -> Token_builder.contents builder)
    | ch when is_octal ch ->
        (* Note we shouldn't skip here as the next char *)
        (* is a part of the number in question. *)
        Lexer_state.with_number_builder st.state Oct @@ fun builder ->
        Token.Oct (consume_number st Oct_builder.is_digit builder)
        |> return_info_thunk st (fun () -> Token_builder.contents builder)
    | ch when is_word_separator ch ->
        Token.Dec (Array.create ~len:1 0L)
        |> return_info_thunk st (fun () -> "0")
    | ch -> Lexer_state.error st.state @@ Wrong_char_in_number_literal ch
  else
    Lexer_state.with_number_builder st.state Dec @@ fun builder ->
    Token_builder.add_char builder ch;
    Token.Dec (consume_number st Dec_builder.is_digit builder)
    |> return_info_thunk st (fun () -> Token_builder.contents builder)

let skip_comments_and_whitespaces st =
  let rec skip_comments_and_whitespaces ~has_advanced st ch =
    match ch with
    | '\t' | ' ' | '\r' ->
        Lexer_state.skip st.state;
        skip_comments_and_whitespaces st
          (Lexer_state.peek st.state)
          ~has_advanced:true
    | '#' ->
        Lexer_state.skip st.state;
        Lexer_state.consume_until_nl st.state;
        true
    | '/' ->
        Lexer_state.skip st.state;
        multiline_comment_start st
          (skip_comments_and_whitespaces ~has_advanced:true)
    | _ -> has_advanced
  in
  skip_comments_and_whitespaces ~has_advanced:false st
    (Lexer_state.peek st.state)

let read_proper_name st builder =
  let open Token_builder in
  Lexer_state.add_to_builder_while_true st.state
    (builder : case_sensitive Token_builder.t)
    (fun ch -> Char.is_valid_name_symbol ch || Char.is_nonascii ch);
  Name (Token_builder.contents builder) |> return_info_simple st

let read_name st builder k =
  Lexer_state.add_to_builder_while_true st.state builder
    Char.is_valid_name_symbol;
  let ch = ref (Lexer_state.peek st.state) in
  if Char.is_nonascii !ch then
    Lexer_state.continue_case_sensitive_builder st.state @@ read_proper_name st
  else (* TODO: what if we return thunks here instead of strings *)
    k (Token_builder.contents builder) (Token_builder.lc_contents builder)

let name_like_started (type t) ~isa_specific ?default st ch =
  let module T = (val st.isa_m : Isa_token.S with type t = t) in
  Lexer_state.with_case_insensitive_builder st.state @@ fun builder ->
  Token_builder.add_char builder ch;
  read_name st builder @@ fun name lc_name ->
  if String.length lc_name <> 1 || Option.is_none default then
    match isa_specific lc_name with
    | None -> Token.Name name |> return_info_simple st
    | Some t -> Token.Isa_specific t |> return_info_thunk st (fun () -> lc_name)
  else Option.value_exn default |> return_info_thunk st (fun () -> lc_name)

let read_escaped k st builder =
  Token_builder.add_char builder
  @@
  match Lexer_state.next st.state with
  | 'a' -> '\x07'
  | 'b' -> '\x08'
  | 't' -> '\t'
  | 'n' -> '\n'
  | 'v' -> '\x0B'
  | 'f' -> '\x0C'
  | 'r' -> '\r'
  | 'x' ->
      let fst = Lexer_state.next st.state in
      let snd = Lexer_state.next st.state in
      Char.of_hex_digits_le fst snd
      |> Option.value_or_thunk ~default:(fun () ->
             Lexer_state.error ~k st.state
             @@ Incorrect_hex_escape_sequence (fst, snd))
  | '"' -> '"'
  | ch -> Lexer_state.error ~k st.state @@ Incorrect_escape_sequence ch

let rec read_string_literal st builder =
  let open Errors in
  let k = Custom (fun () -> Fn.ignore @@ read_string_literal st builder) in
  match Lexer_state.next st.state with
  | '"' ->
      String_literal (Token_builder.contents builder) |> return_info_simple st
  | '\\' ->
      read_escaped k st builder;
      read_string_literal st builder
  | '\n' ->
      Lexer_state.warning ~k st.state Newline_in_string_literal;
      Token_builder.add_char builder '\n';
      read_string_literal st builder
  | '\x00' -> Lexer_state.fail st.state Unfinished_string_literal
  | ch ->
      Token_builder.add_char builder ch;
      read_string_literal st builder

let string_literal_started st =
  Lexer_state.with_case_sensitive_builder st.state @@ read_string_literal st

let recover st =
  match st.recovery with
  | None -> ()
  | Some recovery ->
      st.recovery <- None;
      recovery ()

let next_token (type t) st =
  let module T = (val st.isa_m : Isa_token.S with type t = t) in
  try
    Lexer_state.start_token st.state;
    let return_info_simple = return_info_simple st in
    recover st;
    let has_advanced = skip_comments_and_whitespaces st in
    Option.some
    @@
    if has_advanced then return_info_simple White_space
    else
      let ch = Lexer_state.next st.state in
      match ch with
      | '\n' -> Eol |> return_info_simple
      | '0' .. '9' -> number_start st ch
      | 'A' .. 'Z' | 'a' .. 'z' | '_' ->
          name_like_started ~isa_specific:T.name st ch
      | '.' -> name_like_started ~isa_specific:T.directive ~default:Dot st ch
      | '%' -> name_like_started ~isa_specific:T.reserved ~default:Percent st ch
      | '"' -> string_literal_started st
      | '\x00' -> Eof |> return_info_simple
      | ch when Char.is_nonascii ch ->
          Lexer_state.with_case_sensitive_builder st.state @@ fun builder ->
          Token_builder.add_char builder ch;
          read_proper_name st builder
      | ch ->
          Token.of_special_symbol ch
          |> Option.value_or_thunk ~default:(fun () ->
                 Lexer_state.error st.state @@ Junk_symbol ch)
          |> return_info_simple
  with
  | Diagnostics_handler.Recoverable k ->
      st.recovery <- Some k;
      None
  | Diagnostics_handler.Non_recoverable -> None
  | e -> raise e

let create isa_m inp_m inp dgn_handler_m dgn_handler =
  {
    isa_m;
    state = Lexer_state.create inp_m inp dgn_handler_m dgn_handler;
    recovery = None;
  }

let to_seq lexer =
  let open Sequence.Generator in
  let rec consume_tokens () =
    match next_token lexer with
    | Some (Eof, info) as tok -> yield tok
    | token -> yield token >>= consume_tokens
  in
  run @@ consume_tokens ()

let to_list lexer = Sequence.to_list (to_seq lexer) |> Option.all
