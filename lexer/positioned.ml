open! Import

module type S0 = sig
  type t

  val pos : t -> int * int
  val line : t -> int
  val col : t -> int

  include Equal.S with type t := t
end

module type S0_M = sig
  include S0

  val set_pos : t -> int -> int -> unit
end

module type F = sig
  include S0

  val step : t -> char -> unit
  val step_unchecked : t -> unit
end

module type S = sig
  include F

  val back : t -> char -> unit
  val back_unchecked : t -> unit
end

type 'a t = { unwrap : 'a; mutable line : int; mutable col : int }
[@@deriving fields]

module MakePositionedForward (T : T) = struct
  type nonrec t = T.t t

  let unwrap = unwrap
  let line = line
  let col = col
  let pos x = (line x, col x)
  let equal x y = x.line = y.line && x.col = y.col

  let create ?line ?col unwrap =
    let line = Option.value ~default:1 line in
    let col = Option.value ~default:1 col in
    { unwrap; line; col }

  let set_pos x line col =
    x.line <- line;
    x.col <- col

  let pos = function { unwrap; line; col } -> (line, col)

  let step x ch =
    if Char.is_newline ch then (
      x.line <- x.line + 1;
      x.col <- 1)
    else x.col <- x.col + 1

  let step_unchecked x = x.col <- x.col + 1
end

module MakePositioned (T : T) = struct
  type t = {
    unwrap : T.t;
    mutable line : int;
    mutable col : int;
    prev_line_lens : int Stack.t;
  }
  [@@deriving fields]

  let equal x y = x.line = y.line && x.col = y.col

  let create ?line ?col unwrap =
    let line = Option.value ~default:1 line in
    let col = Option.value ~default:1 col in
    { unwrap; line; col; prev_line_lens = Stack.create () }

  let pos x = (x.line, x.col)

  let step x ch =
    if Char.is_newline ch then (
      Stack.push x.prev_line_lens x.col;
      x.line <- x.line + 1;
      x.col <- 1)
    else x.col <- x.col + 1

  let step_unchecked x = x.col <- x.col + 1

  let back x ch =
    if Char.is_newline ch then (
      let prev = Stack.pop_exn x.prev_line_lens in
      x.line <- x.line - 1;
      x.col <- prev)
    else x.col <- x.col + 1

  let back_unchecked x = x.col <- x.col - 1
end
