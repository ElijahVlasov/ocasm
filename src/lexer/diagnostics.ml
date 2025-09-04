open! Import
include Diagnostics_intf

module Error = struct
  include Diagnostics_intf.Error

  (* We don't really need id's of errors right now. *)
  let id _ = ""

  let show = function
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
end

module Warning = struct
  include Diagnostics_intf.Warning

  let id = function Newline_in_string_literal -> "nl_str_lit"

  let show = function
    | Newline_in_string_literal -> "newline character in a string literal"
end
