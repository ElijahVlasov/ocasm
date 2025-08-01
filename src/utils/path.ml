open Base

type t = string [@@deriving eq]

let empty = ""
let of_string x = x
let to_string x = x
