open! Import

module type S = sig
  type register
  type directive
  type opcode
  type reserved

  type t =
    | Reg of register
    | Opcode of opcode
    | Reserved of reserved
    | Directive of directive

  include Isa_token.S with type t := t
end
