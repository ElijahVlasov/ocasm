open! Import

type ('reg, 'opcode, 'rel) t = {
  mutable opcode : 'opcode option;
  args : ('reg, 'rel) Arg.t array;
  mutable ind : int;
  mutable ty : Isa.Type.t list;
  reg_m : 'reg Isa.Register.t;
  opcode_m : 'opcode Isa.Expr.t;
  word_size : int;
}
[@@deriving fields]

let create ?(len = 3) reg_m opcode_m ~word_size =
  let open Arg in
  {
    opcode = None;
    args = Array.create ~len Uninit;
    ind = 0;
    ty = [];
    reg_m;
    opcode_m;
    word_size;
  }

let unsafe_add_arg st arg rst =
  st.args.(st.ind) <- arg;
  st.ind <- st.ind + 1;
  st.ty <- rst

let rel_bs st =
  let open Relocatable in
  function
  | Imm (bs, _) -> bs
  | Name _ -> word_size st
  | Reloc { bit_size; value; reloc_data } -> bit_size

let add_arg (type reg) st arg =
  let open Arg in
  let open Isa in
  let module R = (val st.reg_m : Isa.Register.S with type t = reg) in
  match (st.ty, arg) with
  | [], _ -> failwith "The opcode is not expecting more arguments"
  | Type.Reg n :: rst, Reg reg ->
      if R.bit_size reg = n then unsafe_add_arg st arg rst
      else failwith "Unexpected register length"
  | Type.Word n :: rst, Rel rel ->
      let bs = rel_bs st rel in
      if bs < n then unsafe_add_arg st arg rst
      else failwith "Incorrect bit length"
  | Type.Base_offset (off_size, base_size) :: rst, Base_offset (off, base) ->
      let off_bs = rel_bs st off in
      let base_bs = R.bit_size base in
      if base_bs < base_size && off_bs < off_size then unsafe_add_arg st arg rst
      else failwith "Something wrong with bit sizes"
  | _, _ -> failwith "Totally incorrect, idk what to do with this"

let add_register st reg = add_arg st (Reg reg)
let add_rel st rel = add_arg st (Rel rel)
let add_base_offset st base off = add_arg st (Base_offset (off, base))

let start (type opcode) st opcode =
  let module O = (val st.opcode_m : Isa.Expr.S with type t = opcode) in
  st.ty <- O.arg_type opcode;
  st.ind <- 0;
  st.opcode <- Some opcode

let build st ~builder = builder (Option.value_exn st.opcode) st.args
