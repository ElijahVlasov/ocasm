open! Import
include Diagnostics_context_intf

(* TODO: this can be a little bit saner. *)
let pp fmt { starts; ends; file; ctx } =
  Stdlib.Format.fprintf fmt "File \"%s\", %a.\n:" (Path.to_string file)
    Location.pp starts
