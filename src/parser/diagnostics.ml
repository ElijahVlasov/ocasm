open! Import
include Diagnostics_intf

module Error = struct
  include Diagnostics_intf.Error

  (* We don't really need id's of errors right now. *)
  let id _ = ""

  let show = function
    | Not_a_command name -> Printf.sprintf "Not a command %s" name
    | Expected_register -> Printf.sprintf "Expected register"
    | Expected tok ->
        Printf.sprintf "Expected %a"
          (fun _ -> Lexer.Token.show (fun _ _ -> ()))
          tok
    | Wrong_register_length (expected, got) -> ""
    | Wrong_word_length (expected, got) -> ""
    | Wrong_base_length (expected, got) -> ""
    | Wrong_offset_length (expected, got) -> ""
    | Too_many_args expected -> ""
end

module Warning = struct
  include Diagnostics_intf.Warning

  let id = function Test -> "test"
  let show = function Test -> "test"
end
