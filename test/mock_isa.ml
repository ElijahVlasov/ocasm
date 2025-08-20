open! Import

module Mock_register = struct
  type t = Reg1 | Reg2 [@@deriving eq, show]

  let to_string = function Reg1 -> "reg1" | Reg2 -> "reg2"

  let of_string = function
    | "reg1" -> Some Reg1
    | "reg2" -> Some Reg2
    | _ -> None

  let bit_size _ = 32
end

module Mock_opcode = struct
  type t = Opcode1 | Opcode2 [@@deriving eq, show]

  let to_string = function Opcode1 -> "opcode1" | Opcode2 -> "opcode2"

  let of_string = function
    | "opcode1" -> Some Opcode1
    | "opcode2" -> Some Opcode2
    | _ -> None

  let arg_type = function
    | Opcode1 -> [ Isa.Type.Reg 32; Isa.Type.Reg 32 ]
    | Opcode2 -> [ Isa.Type.Reg 32; Isa.Type.Reg 32 ]
end

module Mock_dir = struct
  type t = Dir1 | Dir2 [@@deriving eq, show]

  let to_string = function Dir1 -> "dir1" | Dir2 -> "dir2"

  let of_string = function
    | "dir1" -> Some Dir1
    | "dir2" -> Some Dir2
    | _ -> None

  let arg_type = function
    | Dir1 -> [ Isa.Type.String ]
    | Dir2 -> [ Isa.Type.String ]
end

module Mock_reserved = struct
  type t = unit [@@deriving eq, show]

  let to_string _ = ""
  let of_string _ = None
  let arg_type _ = []
end

module Mock_token = struct
  open Isa.Token
  open Mock_register
  open Mock_dir
  open Mock_opcode

  type t =
    (Mock_register.t, Mock_dir.t, Mock_opcode.t, Mock_reserved.t) Isa.Token.t

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
end

module Mock_instruction = struct
  type t =
    | Opcode1 of (Mock_register.t * Mock_register.t)
    | Opcode2 of (Mock_register.t * Mock_register.t)
  [@@deriving eq, show]
end

module Mock_directive = struct
  type t = Dir1 of string | Dir2 of string [@@deriving eq, show]
end
