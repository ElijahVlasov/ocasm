open! Import
include Diagnostics_intf

module Error = struct
  include Diagnostics_intf.Error

  (* We don't really need id's of errors right now. *)
  let id _ = ""

  let show = function
    | Not_a_command name -> Printf.sprintf "Not a command %s" name
end

module Warning = struct
  include Diagnostics_intf.Warning

  let id = function Test -> "test"
  let show = function Test -> "test"
end
