type fence_type = int [@@deriving eq]

let i = 8
let o = 4
let r = 2
let w = 1

let show_fence_type a =
  let i = if a land i = i then "i" else "" in
  let o = if a land o = o then "o" else "" in
  let r = if a land r = r then "r" else "" in
  let w = if a land w = w then "w" else "" in
  i ^ o ^ r ^ w

let equal_fence_type a b = a = b
let pp_fence_type fmt a = Format.fprintf fmt "%s" (show_fence_type a)
let ( |+ ) (a : fence_type) (b : fence_type) : fence_type = a lor b
let to_int32 a = Int32.of_int a
