type register

val of_int : int -> register option
val of_int32 : int32 -> register option
val to_int : register -> int
val to_int32 : register -> int32
val to_string : register -> string
val of_string : string -> register option
val equal_register : register -> register -> bool
val pp_register : Format.formatter -> register -> unit
val x0 : register
val x1 : register
val x2 : register
val x3 : register
val x4 : register
val x5 : register
val x6 : register
val x7 : register
val x8 : register
val x9 : register
val x10 : register
val x11 : register
val x12 : register
val x13 : register
val x14 : register
val x15 : register
val x16 : register
val x17 : register
val x18 : register
val x19 : register
val x20 : register
val x21 : register
val x22 : register
val x23 : register
val x24 : register
val x25 : register
val x26 : register
val x27 : register
val x28 : register
val x29 : register
val x30 : register
val x31 : register
val x32 : register
val pc : register
val zero : register
val ra : register
val sp : register
val gp : register
val tp : register
val t0 : register
val t1 : register
val t2 : register
val s0 : register
val s1 : register
val a0 : register
val a1 : register
val a2 : register
val a3 : register
val a4 : register
val a5 : register
val a6 : register
val a7 : register
val s2 : register
val s3 : register
val s4 : register
val s5 : register
val s6 : register
val s7 : register
val s8 : register
val s9 : register
val s10 : register
val s11 : register
