open! Import

type 'a t = { unwrap : 'a; mutable line : int; mutable col : int }
[@@deriving eq, fields]

let with_value f { unwrap; line; col } = f unwrap

let create ?line ?col unwrap =
  let line = Option.value ~default:1 line in
  let col = Option.value ~default:1 col in
  { unwrap; line; col }

let pos = function { unwrap; line; col } -> Location.create line col

let step x ch =
  if Char.is_newline ch then (
    x.line <- x.line + 1;
    x.col <- 1)
  else x.col <- x.col + 1

let step_unchecked x = x.col <- x.col + 1
