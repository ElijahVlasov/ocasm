open! Import

type t = private string [@@deriving eq, show, compare, hash, sexp]

val of_string : string -> t
