open Base
open Ocasm_utils

module Message = struct
  type t = {
    msg : string;
    id : int;
    left : int * int;
    right : int * int;
    file : Path.t;
    ctx : string;
  }
end

type t = { filter : int Hash_set.t; promote : int Hash_set.t }

let create ?filter ?promote () =
  let filter =
    Option.value_or_thunk filter ~default:(fun () ->
        Hash_set.create (module Int))
  in
  let promote =
    Option.value_or_thunk promote ~default:(fun () ->
        Hash_set.create (module Int))
  in
  { filter; promote }

type diagnostic_type = Warning | Error [@@deriving show]

let print_diagnostic_message msg ty =
  let open Message in
  let open Stdlib.Printf in
  let open Stdlib.Out_channel in
  let file = Path.to_string msg.file in
  let line, col = msg.left in
  let ty = show_diagnostic_type ty in
  let msg = msg.msg in
  fprintf stderr "File \"%s\", line %d, column %d.\n%s: %s" file line col ty msg

exception Recoverable of (unit -> unit)
exception Non_recoverable

let error ?recovery st msg =
  print_diagnostic_message msg Error;
  raise
  @@
  match recovery with
  | None -> Non_recoverable
  | Some recovery -> Recoverable recovery

let warn ?recovery st msg =
  let open Message in
  if not @@ Hash_set.mem st.filter msg.id then
    if Hash_set.mem st.promote msg.id then error ?recovery st msg
    else print_diagnostic_message msg Warning
