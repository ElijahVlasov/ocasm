open! Import
include Diagnostic_message_intf

module Diagnostic_type = struct
  type 'a t = 'a diagnostic_type

  let pp (type a) fmt (ty : a t) =
    Stdlib.Format.fprintf fmt "%s"
    @@ match ty with Warning -> "Warning" | Error -> "Error"
end

let pp fmt { typ; msg; starts; ends; file; ctx } =
  Stdlib.Format.fprintf fmt "File \"%s\", %a.\n%a: %s" (Path.to_string file)
    Location.pp starts Diagnostic_type.pp typ msg

module Dyn = struct
  type 'a msg = 'a t
  type t = dyn

  let pp fmt = function Dyn (type a) (msg : a msg) -> pp fmt msg

  let equal_aux x y =
    String.equal x.msg y.msg
    && Location.equal x.starts y.starts
    && Location.equal x.ends y.ends
    && Path.equal x.file y.file && String.equal x.ctx y.ctx

  let equal x y =
    match (x, y) with
    | Dyn (type a) (x : a msg), Dyn (type b) (y : b msg) -> (
        match (x.typ, y.typ) with
        | Error, Error | Warning, Warning -> equal_aux x y
        | _ -> false)
end
