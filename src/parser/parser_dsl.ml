open! Import
include Parser_dsl_intf

type ('reg, 'dir, 'opcode, 'res) t = {
  reg_m : 'reg Isa.Register.t;
  dir_m : 'dir Isa.Expr.t;
  opcode_m : 'opcode Isa.Expr.t;
  res_m : 'res Isa.Expr.t;
  path : Path.t;
  mutable state : Parser_state.t;
}
[@@deriving fields]

module Selector = struct
  type nonrec ('reg, 'dir, 'opcode, 'res, 'a) t =
    ('reg, 'dir, 'opcode, 'res) t -> 'a Isa.Expr.t

  let dir = dir_m
  let res = res_m
  let opcode = opcode_m
end

let create ?(path = Path.empty) reg_m dir_m opcode_m res_m =
  { reg_m; dir_m; opcode_m; res_m; path; state = Parser_state.Initial }

let arg_type (type a) st sel expr =
  let module E = (val sel st : Isa.Expr.S with type t = a) in
  E.arg_type expr
