open! Import

type t =
  | X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X16
  | X17
  | X18
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X28
  | X29
  | X30
  | X31
  | X32
[@@deriving eq, show]

let of_string s =
  match s with
  | "x0" -> Some X0
  | "x1" -> Some X1
  | "x2" -> Some X2
  | "x3" -> Some X3
  | "x4" -> Some X4
  | "x5" -> Some X5
  | "x6" -> Some X6
  | "x7" -> Some X7
  | "x8" -> Some X8
  | "x9" -> Some X9
  | "X10" -> Some X10
  | "X11" -> Some X11
  | "X12" -> Some X12
  | "X13" -> Some X13
  | "X14" -> Some X14
  | "X15" -> Some X15
  | "X16" -> Some X16
  | "X17" -> Some X17
  | "X18" -> Some X18
  | "X19" -> Some X19
  | "X20" -> Some X20
  | "X21" -> Some X21
  | "X22" -> Some X22
  | "X23" -> Some X23
  | "X24" -> Some X24
  | "X25" -> Some X25
  | "X26" -> Some X26
  | "X27" -> Some X27
  | "X28" -> Some X28
  | "X29" -> Some X29
  | "X30" -> Some X30
  | "X31" -> Some X31
  | "X32" -> Some X32
  | _ -> None
