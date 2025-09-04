open! Import

type t = { starts : Location.t; ends : Location.t; string : string }
[@@deriving eq, show]

let to_string = show

let default_value =
  { starts = Location.default (); ends = Location.default (); string = "" }

let default () = default_value
