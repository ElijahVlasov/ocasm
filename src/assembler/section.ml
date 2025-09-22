open! Import

type t = string [@@deriving eq, show]

let of_string x =
  match x.(0) with
  | '.' -> x
  | _ -> failwith "A section name has to begin with \".\""
