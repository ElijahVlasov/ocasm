open! Import

type t = string [@@deriving eq, show, compare, hash, sexp]

let of_string x =
  match String.get x 0 with
  | '.' -> x
  | _ -> failwith "A section name has to begin with \".\""
