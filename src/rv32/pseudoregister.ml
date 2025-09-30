open! Import

type t =
  | Pc
  | Zero
  | Ra
  | Sp
  | Gp
  | Tp
  | T0
  | T1
  | T2
  | S0
  | S1
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | T3
  | T4
  | T5
  | T6
[@@deriving eq, show]

let of_string s =
  match s with
  | "pc" -> Some Pc
  | "zero" -> Some Zero
  | "ra" -> Some Ra
  | "sp" -> Some Sp
  | "gp" -> Some Gp
  | "tp" -> Some Tp
  | "t0" -> Some T0
  | "t1" -> Some T1
  | "t2" -> Some T2
  | "s0" -> Some S0
  | "s1" -> Some S1
  | "a0" -> Some A0
  | "a1" -> Some A1
  | "a2" -> Some A2
  | "a3" -> Some A3
  | "a4" -> Some A4
  | "a5" -> Some A5
  | "a6" -> Some A6
  | "a7" -> Some A7
  | "s2" -> Some S2
  | "s3" -> Some S3
  | "s4" -> Some S4
  | "s5" -> Some S5
  | "s6" -> Some S6
  | "s7" -> Some S7
  | "s8" -> Some S8
  | "s9" -> Some S9
  | "s10" -> Some S10
  | "s11" -> Some S11
  | "t3" -> Some T3
  | "t4" -> Some T4
  | "t5" -> Some T5
  | "t6" -> Some T6
  | _ -> None
