open! Import
include Diagnostics_handler_intf

module Kitchen_sink_handler = struct
  type t = (Diagnostics.Any.t * Diagnostics_context.t) Queue.t

  let create ?filter ?promote ~pp_err ~pp_warn () = Queue.create ()

  let error (type a) ?recovery h err context : a =
    Queue.enqueue h (Diagnostics.Any.Error err, context);
    raise
    @@
    match recovery with
    | None -> Non_recoverable
    | Some recovery -> Recoverable recovery

  let warn ?recovery h warning context =
    Queue.enqueue h (Diagnostics.Any.Warning warning, context);
    ()

  let to_list h = Queue.to_list h
end

module Std_handler = struct
  type t = {
    filter : Diagnostics.Warning.t -> bool;
    promote : Diagnostics.Warning.t -> bool;
    pp_err : (module Pretty_printer.S with type t = Diagnostics.Error.t);
    pp_warn : (module Pretty_printer.S with type t = Diagnostics.Warning.t);
  }

  let create ?(filter = fun _ -> false) ?(promote = fun _ -> false) ~pp_err
      ~pp_warn () =
    { filter; promote; pp_err; pp_warn }

  let print_message title pp msg context : unit = Panic.unimplemented ()

  let error_aux recovery err pp context =
    print_message "Error" pp err context;
    raise
    @@
    match recovery with
    | None -> Non_recoverable
    | Some recovery -> Recoverable recovery

  let error ?recovery h err context =
    let module Pp =
      (val h.pp_err : Pretty_printer.S with type t = Diagnostics.Error.t)
    in
    error_aux recovery err Pp.pp context

  let warn ?recovery h warn context =
    let module Pp =
      (val h.pp_warn : Pretty_printer.S with type t = Diagnostics.Warning.t)
    in
    if not @@ h.filter warn then
      if h.promote warn then error_aux recovery warn Pp.pp context
      else print_message "Warning" Pp.pp warn context
end
