open! Import

type t = {
  filter : string Hash_set.t;
  promote : string Hash_set.t;
  fmt : Formatter.t;
}

let create ?(fmt = Stdlib.Format.std_formatter)
    ?(filter = Hash_set.create ~growth_allowed:false ~size:0 (module String))
    ?(promote = Hash_set.create ~growth_allowed:false ~size:0 (module String))
    () =
  { filter; promote; fmt }

let emit p msg err =
  let open Diagnostics_message in
  let open Diagnostics_type in
  let open Stdlib.Format in
  fprintf p.fmt "%a" Diagnostics_message.pp msg;
  match msg.ty with
  | Warning ->
      if Hash_set.mem p.filter msg.id then Ok ()
      else if Hash_set.mem p.promote msg.id then Error err
      else Ok ()
  | Error -> Error err
