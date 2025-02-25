type register = int32

let of_int n = if n < 0 || n > 31 then None else Some (Int32.of_int n)
let of_int32 n = if n < 0l || n > 31l then None else Some n
let to_int n = Int32.to_int n
let to_int32 n = n
let to_string n = Printf.sprintf "x%d" (to_int n)
let pp_register fmt n = Format.fprintf fmt "%s" (to_string n)
let equal_register r1 r2 = r1 = r2
let x0 : register = 0l
let x1 : register = 1l
let x2 : register = 2l
let x3 : register = 3l
let x4 : register = 4l
let x5 : register = 5l
let x6 : register = 6l
let x7 : register = 7l
let x8 : register = 8l
let x9 : register = 9l
let x10 : register = 10l
let x11 : register = 11l
let x12 : register = 12l
let x13 : register = 13l
let x14 : register = 14l
let x15 : register = 15l
let x16 : register = 16l
let x17 : register = 17l
let x18 : register = 18l
let x19 : register = 19l
let x20 : register = 20l
let x21 : register = 21l
let x22 : register = 22l
let x23 : register = 23l
let x24 : register = 24l
let x25 : register = 25l
let x26 : register = 26l
let x27 : register = 27l
let x28 : register = 28l
let x29 : register = 29l
let x30 : register = 30l
let x31 : register = 31l
let x32 : register = 32l
let pc : register = x32
let zero : register = x0
let ra : register = x1
let sp : register = x2
let gp : register = x3
let tp : register = x4
let t0 : register = x5
let t1 : register = x6
let t2 : register = x7
let s0 : register = x8
let s1 : register = x9
let a0 : register = x10
let a1 : register = x11
let a2 : register = x12
let a3 : register = x13
let a4 : register = x14
let a5 : register = x15
let a6 : register = x16
let a7 : register = x17
let s2 : register = x18
let s3 : register = x19
let s4 : register = x20
let s5 : register = x21
let s6 : register = x22
let s7 : register = x23
let s8 : register = x24
let s9 : register = x25
let s10 : register = x26
let s11 : register = x27
let t3 : register = x28
let t4 : register = x29
let t5 : register = x30
let t6 : register = x31

let of_string s =
  match s with
  | "x0" -> Some x0
  | "x1" -> Some x1
  | "x2" -> Some x2
  | "x3" -> Some x3
  | "x4" -> Some x4
  | "x5" -> Some x5
  | "x6" -> Some x6
  | "x7" -> Some x7
  | "x8" -> Some x8
  | "x9" -> Some x9
  | "x10" -> Some x10
  | "x11" -> Some x11
  | "x12" -> Some x12
  | "x13" -> Some x13
  | "x14" -> Some x14
  | "x15" -> Some x15
  | "x16" -> Some x16
  | "x17" -> Some x17
  | "x18" -> Some x18
  | "x19" -> Some x19
  | "x20" -> Some x20
  | "x21" -> Some x21
  | "x22" -> Some x22
  | "x23" -> Some x23
  | "x24" -> Some x24
  | "x25" -> Some x25
  | "x26" -> Some x26
  | "x27" -> Some x27
  | "x28" -> Some x28
  | "x29" -> Some x29
  | "x30" -> Some x30
  | "x31" -> Some x31
  | "x32" -> Some x32
  | "pc" -> Some pc
  | "zero" -> Some zero
  | "ra" -> Some ra
  | "sp" -> Some sp
  | "gp" -> Some gp
  | "tp" -> Some tp
  | "t0" -> Some t0
  | "t1" -> Some t1
  | "t2" -> Some t2
  | "s0" -> Some s0
  | "s1" -> Some s1
  | "a0" -> Some a0
  | "a1" -> Some a1
  | "a2" -> Some a2
  | "a3" -> Some a3
  | "a4" -> Some a4
  | "a5" -> Some a5
  | "a6" -> Some a6
  | "a7" -> Some a7
  | "s2" -> Some s2
  | "s3" -> Some s3
  | "s4" -> Some s4
  | "s5" -> Some s5
  | "s6" -> Some s6
  | "s7" -> Some s7
  | "s8" -> Some s8
  | "s9" -> Some s9
  | "s10" -> Some s10
  | "s11" -> Some s11
  | "t3" -> Some t3
  | "t4" -> Some t4
  | "t5" -> Some t5
  | "t6" -> Some t6
  | _ -> None
