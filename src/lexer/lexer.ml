open! Import
include Lexer_intf

type ('a, 't) t = {
  isa_m : 't Isa_token.t;
      (** First-class module related to the ISA-specific tokens the lexer is
          supposed to work with. *)
  dsl : 'a Lexer_dsl.t;
      (** Low-level lexer state (position in the file, errors, token builders).
      *)
  mutable no_errors : bool;
}
[@@deriving fields]
(** The main lexer state type. The main component is the lexer state. *)

open Result.Let_syntax

(** Prepare token info for [token] by supplying [fun () -> T.to_string token].
*)
let return_info_simple (type t) st token =
  let module T = Token.MkToken ((val st.isa_m : Isa_token.S with type t = t)) in
  let open Token_info in
  return
    ( token,
      {
        starts = Lexer_dsl.get_start st.dsl;
        ends = Lexer_dsl.pos st.dsl;
        string = T.to_string token;
      } )

(** Now supply a proper thunk. *)
let return_info_real st token_real token =
  let open Token_info in
  return
    ( token,
      {
        starts = Lexer_dsl.get_start st.dsl;
        ends = Lexer_dsl.pos st.dsl;
        string = token_real;
      } )

let rec multiline_comment st k =
  let open Char in
  let ch = Lexer_dsl.next st.dsl in
  if ch = '*' then
    (* We're potentially wrapping up the comment *)
    if Lexer_dsl.peek st.dsl = '/' then (
      Lexer_dsl.skip st.dsl;
      k st (Lexer_dsl.next st.dsl))
    else multiline_comment st k
  else if is_eof ch then Lexer_dsl.error st.dsl Unfinished_comment
  else multiline_comment st k

let multiline_comment_start st k =
  let open Char in
  let next_ch = Lexer_dsl.next st.dsl in
  if next_ch = '*' then multiline_comment st k
  else Lexer_dsl.error st.dsl @@ Expected_vs_got ('*', next_ch)

let consume_number st is_digit builder =
  Lexer_dsl.add_to_builder_while_true st.dsl builder is_digit;
  let ch = Lexer_dsl.peek st.dsl in
  if Char.is_word_separator ch then return @@ Token_builder.to_number builder
  else Lexer_dsl.error st.dsl @@ Wrong_char_in_number_literal ch

let number_start st ch =
  let open Char in
  let open Number_builder in
  if ch = '0' then
    match lowercase (Lexer_dsl.peek st.dsl) with
    | 'x' ->
        Lexer_dsl.skip st.dsl;
        Lexer_dsl.with_number_builder st.dsl Hex @@ fun builder ->
        let%bind number = consume_number st Hex_builder.is_digit builder in
        Token.Hex number |> return_info_real st (Token_builder.contents builder)
    | 'b' ->
        Lexer_dsl.skip st.dsl;
        Lexer_dsl.with_number_builder st.dsl Bin @@ fun builder ->
        let%bind number = consume_number st Bin_builder.is_digit builder in
        Token.Bin number |> return_info_real st (Token_builder.contents builder)
    | ch when is_octal ch ->
        (* Note we shouldn't skip here as the next char *)
        (* is a part of the number in question. *)
        Lexer_dsl.with_number_builder st.dsl Oct @@ fun builder ->
        let%bind number = consume_number st Oct_builder.is_digit builder in
        Token.Oct number |> return_info_real st (Token_builder.contents builder)
    | ch when is_word_separator ch ->
        Token.Dec Big_integer.zero |> return_info_real st "0"
    | ch -> Lexer_dsl.error st.dsl @@ Wrong_char_in_number_literal ch
  else
    Lexer_dsl.with_number_builder st.dsl Dec @@ fun builder ->
    Token_builder.add_char builder ch;
    let%bind number = consume_number st Dec_builder.is_digit builder in
    Token.Dec number |> return_info_real st (Token_builder.contents builder)

let skip_comments_and_whitespaces st =
  let rec skip_comments_and_whitespaces ~has_advanced st ch =
    match ch with
    | '\t' | ' ' | '\r' ->
        Lexer_dsl.skip st.dsl;
        skip_comments_and_whitespaces st (Lexer_dsl.peek st.dsl)
          ~has_advanced:true
    | '#' ->
        Lexer_dsl.skip st.dsl;
        Lexer_dsl.consume_until_nl st.dsl;
        return true
    | '/' ->
        Lexer_dsl.skip st.dsl;
        multiline_comment_start st
          (skip_comments_and_whitespaces ~has_advanced:true)
    | _ -> return has_advanced
  in
  skip_comments_and_whitespaces ~has_advanced:false st (Lexer_dsl.peek st.dsl)

let read_proper_name st (builder : Token_builder.case_sensitive Token_builder.t)
    =
  Lexer_dsl.add_to_builder_while_true st.dsl builder (fun ch ->
      Char.is_valid_name_symbol ch || Char.is_nonascii ch);
  Name (Token_builder.contents builder) |> return_info_simple st

let read_name st builder k =
  Lexer_dsl.add_to_builder_while_true st.dsl builder Char.is_valid_name_symbol;
  let ch = ref (Lexer_dsl.peek st.dsl) in
  if Char.is_nonascii !ch then
    Lexer_dsl.continue_case_sensitive_builder st.dsl @@ read_proper_name st
  else (* TODO: what if we return thunks here instead of strings *)
    k (Token_builder.contents builder) (Token_builder.lc_contents builder)

let name_like_started (type t) ~isa_specific ?default st ch =
  let module T = (val st.isa_m : Isa_token.S with type t = t) in
  Lexer_dsl.with_case_insensitive_builder st.dsl @@ fun builder ->
  Token_builder.add_char builder ch;
  read_name st builder @@ fun name lc_name ->
  if String.length lc_name <> 1 || Option.is_none default then
    match isa_specific lc_name with
    | None -> Token.Name name |> return_info_simple st
    | Some t -> Token.Isa_specific t |> return_info_real st lc_name
  else Option.value_exn default |> return_info_real st lc_name

let read_escaped st builder =
  let%map char =
    match Lexer_dsl.next st.dsl with
    | 'a' -> return '\x07'
    | 'b' -> return '\x08'
    | 't' -> return '\t'
    | 'n' -> return '\n'
    | 'v' -> return '\x0B'
    | 'f' -> return '\x0C'
    | 'r' -> return '\r'
    | 'x' -> (
        let fst = Lexer_dsl.next st.dsl in
        let snd = Lexer_dsl.next st.dsl in
        match Char.of_hex_digits_le fst snd with
        | None ->
            Lexer_dsl.error st.dsl @@ Incorrect_hex_escape_sequence (fst, snd)
        | Some char -> return char)
    | '"' -> return '"'
    | ch -> Lexer_dsl.error st.dsl @@ Incorrect_escape_sequence ch
  in
  Token_builder.add_char builder char

let rec read_string_literal st builder =
  match Lexer_dsl.next st.dsl with
  | '"' ->
      String_literal (Token_builder.contents builder) |> return_info_simple st
  | '\\' ->
      let%bind () = read_escaped st builder in
      read_string_literal st builder
  | '\n' ->
      let%bind () = Lexer_dsl.warning st.dsl Newline_in_string_literal in
      Token_builder.add_char builder '\n';
      read_string_literal st builder
  | '\x00' -> Lexer_dsl.error st.dsl Unfinished_string_literal
  | ch ->
      Token_builder.add_char builder ch;
      read_string_literal st builder

let string_literal_started st =
  Lexer_dsl.with_case_sensitive_builder st.dsl @@ read_string_literal st

let next_aux (type t) st =
  let module T = (val st.isa_m : Isa_token.S with type t = t) in
  Lexer_dsl.start_token st.dsl;
  let%bind has_advanced = skip_comments_and_whitespaces st in
  if has_advanced then return_info_simple st White_space
  else
    let ch = Lexer_dsl.next st.dsl in
    match ch with
    | '\n' -> return_info_simple st Eol
    | '0' .. '9' -> number_start st ch
    | 'A' .. 'Z' | 'a' .. 'z' | '_' ->
        name_like_started ~isa_specific:T.name st ch
    | '.' -> name_like_started ~isa_specific:T.directive ~default:Dot st ch
    | '%' -> name_like_started ~isa_specific:T.reserved ~default:Percent st ch
    | '"' -> string_literal_started st
    | '\x00' -> return_info_simple st Eof
    | ch when Char.is_nonascii ch ->
        Lexer_dsl.with_case_sensitive_builder st.dsl @@ fun builder ->
        Token_builder.add_char builder ch;
        read_proper_name st builder
    | ch -> (
        match Token.of_special_symbol ch with
        | None -> Lexer_dsl.error st.dsl @@ Junk_symbol ch
        | Some token -> return_info_simple st token)

let recover_aux st =
  let open Either in
  let open Diagnostics.Error in
  let open Diagnostics.Warning in
  function
  | Second Newline_in_string_literal -> Panic.unimplemented ()
  | First (Incorrect_escape_sequence ch) -> Panic.unimplemented ()
  | First (Incorrect_hex_escape_sequence (fst, snd)) -> Panic.unimplemented ()
  | First (Wrong_char_in_number_literal _)
  | First (Expected_vs_got _)
  | First (Junk_symbol _) ->
      Lexer_dsl.consume_until_nl st.dsl;
      return ()
  (* In these two we have already reached the EOF. *)
  | First Unfinished_comment | First Unfinished_string_literal -> return ()

let rec recover st err =
  match recover_aux st err with Ok () -> () | Error err -> recover st err

let rec next st =
  match next_aux st with
  | Ok next -> next
  | Error err ->
      st.no_errors <- false;
      recover st err;
      next st

let create isa_m inp_m inp dgn_printer =
  { isa_m; dsl = Lexer_dsl.create inp_m inp dgn_printer; no_errors = true }

let to_seq lexer =
  let open Sequence.Generator in
  let rec consume_tokens () =
    match next lexer with
    | (Eof, info) as tok_info -> yield tok_info
    | tok_info -> yield tok_info >>= consume_tokens
  in
  run @@ consume_tokens ()

let to_list lexer = Sequence.to_list (to_seq lexer)
