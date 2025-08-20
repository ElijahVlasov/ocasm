open! Base
open Ocasm_utils

module Type = struct
  type t =
    | Reg of int  (** [Reg n] denotes a register of bit size [n]. *)
    | Word of int  (** An immediate or a symbol. *)
    | Base_offset of int * int
        (** [Base_offset off_size base_size] is a type of expressions
            [off(reg)], where [off] is an offset of size [off_size] and [reg] is
            a register of size base_size. *)
    | String  (** String literal. *)
  [@@deriving eq]
end

module Register = struct
  module type S = sig
    type t

    include Equal.S with type t := t
    include To_string.S with type t := t
    include Pretty_printer.S with type t := t

    val of_string : string -> t option
    val bit_size : t -> int
  end

  type 'a t = (module S with type t = 'a)
end

module Expr = struct
  module type S = sig
    type t

    include Equal.S with type t := t
    include To_string.S with type t := t
    include Pretty_printer.S with type t := t

    val of_string : string -> t option
    val arg_type : t -> Type.t list
  end

  type 'a t = (module S with type t = 'a)
end

module Token = struct
  type ('reg, 'dir, 'opcode, 'res) t =
    | Reg of 'reg
    | Dir of 'dir
    | Opcode of 'opcode
    | Res of 'res
  [@@deriving eq, show]
end

module type S = sig
  module Directive : Expr.S
  module Opcode : Expr.S
  module Reserved : Expr.S
  module Register : Register.S
end
