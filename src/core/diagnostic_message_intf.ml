open Base
open Ocasm_utils

type 'a diagnostic_type =
  | Warning : unit diagnostic_type
  | Error : 'a diagnostic_type

type 'a t = {
  typ : 'a diagnostic_type;
  msg : string;
  id : int;
  starts : Location.t;
  ends : Location.t;
  file : Path.t;
  ctx : string;
}

type dyn = Dyn : 'a t -> dyn
