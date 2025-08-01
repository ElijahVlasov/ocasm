open Import

type recovery = No_recovery | Read_to_eol | Custom of (unit -> unit)

type error =
  | Incorrect_escape_sequence of char
  | Incorrect_hex_escape_sequence of char * char
  | Unfinished_comment
  | Unfinished_string_literal
  | Expected_vs_got of char * char
  | Wrong_char_in_number_literal of char
  | Junk_symbol of char

type warning = Newline_in_string_literal

module To_diagnostic_message = struct
  module type S = sig
    type 'a t

    val to_diagnostic_message :
      'a t ->
      Location.t ->
      Location.t ->
      Path.t ->
      string ->
      'a Diagnostic_message.t
  end
end
