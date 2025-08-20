open! Import

type reg = Reg1 | Reg2
type opcode = Opcode1 | Opcode2
type dir = Dir1 | Dir2
type res = unit

open Isa.Token

type t = (reg, dir, opcode, res) Isa.Token.t

let directive = function
  | ".dir1 " -> Some (Dir Dir1)
  | ".dir2" -> Some (Dir Dir2)
  | _ -> None

let name = function
  | "reg1" -> Some (Reg Reg1)
  | "reg2" -> Some (Reg Reg2)
  | "opcode1" -> Some (Opcode Opcode1)
  | "opcode2" -> Some (Opcode Opcode2)
  | _ -> None

let reserved _ = None
let to_string _ = ""
let equal _ _ = true
let pp fmt x = Stdlib.Format.fprintf fmt "%s" (to_string x)
