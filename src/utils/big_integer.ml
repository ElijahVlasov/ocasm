open! Import

type t = int64 array [@@deriving eq, show]

exception Too_big_for_int64

let zero = Array.create ~len:1 0L
let of_int64 = Array.create ~len:1
let of_int x = Int64.of_int x |> of_int64
let of_int32 x = Int64.of_int32 x |> of_int64

(* TODO: Change this to handle arbitrary long numbers. *)
let of_buffer x = Buffer.contents x |> Int64.of_string |> of_int64

let to_int64_exn x =
  if Array.length x = 1 then x.(0) else raise Too_big_for_int64
