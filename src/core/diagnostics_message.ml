open! Import
include Diagnostics_message_intf

let pp fmt { id; msg; ty; starts; ends; file; ctx } =
  let open Stdlib.Format in
  fprintf fmt "%a: %s.\nFile \"%s\", %a, %a.\n" Diagnostics_type.pp ty msg
    (Path.to_string file) Location.pp starts Location.pp ends
