open Base
include Diagnostics_handler_intf

module Kitchen_sink_handler = struct
  open Diagnostic_message

  type t = Dyn.t Queue.t

  let create ?filter ?promote () = Queue.create ()

  let throw (type a) ?recovery handler (msg : a Diagnostic_message.t) : a =
    Queue.enqueue handler (Dyn msg);
    let error () =
      raise
      @@
      match recovery with
      | None -> Non_recoverable
      | Some recovery -> Recoverable recovery
    in
    match msg.typ with Warning -> () | Error -> error ()

  let to_list handler = Queue.to_list handler
end

module Std_handler = struct
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

  let throw (type a) ?recovery handler (msg : a Diagnostic_message.t) : a =
    let error () =
      raise
      @@
      match recovery with
      | None -> Non_recoverable
      | Some recovery -> Recoverable recovery
    in
    match msg.typ with
    | Warning ->
        if not @@ Hash_set.mem handler.filter msg.id then
          if Hash_set.mem handler.promote msg.id then error ()
          else Diagnostic_message.pp Stdlib.Format.std_formatter msg
    | Error -> error ()
end
