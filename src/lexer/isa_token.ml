open! Import
include Isa_token_intf

module OF_ISA (I : Isa.S) = struct
  open Option
  open Isa.Token
  include I

  type t = (I.Register.t, I.Directive.t, I.Opcode.t, I.Reserved.t) Isa.Token.t

  let equal =
    Isa.Token.equal I.Register.equal I.Directive.equal I.Opcode.equal
      I.Reserved.equal

  let pp = Isa.Token.pp I.Register.pp I.Directive.pp I.Opcode.pp I.Reserved.pp
  let dir x = Dir x
  let reg x = Reg x
  let opcode x = Opcode x
  let res x = Res x
  let directive str = Directive.of_string str >>| dir
  let reserved str = Reserved.of_string str >>| res

  let name str =
    match Register.of_string str with
    | None -> (
        match Opcode.of_string str with
        | None -> None
        | Some o -> Some (opcode o))
    | Some r -> Some (reg r)

  let to_string x =
    match x with
    | Reg reg -> Register.to_string reg
    | Dir dir -> Directive.to_string dir
    | Opcode op -> Opcode.to_string op
    | Res res -> Reserved.to_string res
end
