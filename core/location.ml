open Base

type t = int * int

let create line col = (line, col)
let equal x y = fst x = fst y && snd x = snd y
let pp fmt x = Stdlib.Format.fprintf fmt "line = %d, col = %d" (fst x) (snd x)
