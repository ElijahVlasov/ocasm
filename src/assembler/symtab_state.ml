open Base
open Base.Fn

type 'a t = (string, 'a Symbol.t) Hashtbl.t

let create _ = Hashtbl.create (module String)
let finalize = id

exception DuplicateSymbol of string

let add_symbol tbl sym =
  let open Symbol in
  let key = sym.name in
  match Hashtbl.add tbl ~key ~data:sym with
  | `Ok -> ()
  | `Duplicate -> raise @@ DuplicateSymbol key
