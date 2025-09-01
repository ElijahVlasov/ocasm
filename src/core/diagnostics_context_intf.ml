open! Import

type t = { starts : Location.t; ends : Location.t; file : Path.t; ctx : string }
