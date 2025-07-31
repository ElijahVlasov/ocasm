open Base
open Core
include Errors_intf

module Recovery = struct
  type t = recovery
end

module Error = struct
  type 'a t = Error : error -> 'a t

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

  let to_diagnostic_message (type a) (err : a t) starts ends file ctx =
    match err with
    | Error err ->
        let open Diagnostic_message in
        {
          typ = Diagnostic_message.Error;
          msg = msg err;
          id = id err;
          starts;
          ends;
          file;
          ctx;
        }
end

module Warning = struct
  type 'a t = Warning : warning -> unit t

  let msg = function
    | Newline_in_string_literal -> "newline character in a string literal"

  let id = function Newline_in_string_literal -> 0

  let to_diagnostic_message (type a) (warn : a t) starts ends file ctx =
    match warn with
    | Warning warn ->
        let open Diagnostic_message in
        {
          typ = Diagnostic_message.Error;
          msg = msg warn;
          id = id warn;
          starts;
          ends;
          file;
          ctx;
        }
end
