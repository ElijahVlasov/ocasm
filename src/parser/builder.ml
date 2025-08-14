open! Import
include Builder_intf

type ('reg, 'comm, 'rel, 'a) t = {
  mutable comm : 'comm option;
  args : ('reg, 'rel) Arg.t array;
  mutable ind : int;
  mutable ty : Isa.Type.t list;
  reg_m : 'reg Isa.Register.t;
  comm_m : 'comm Isa.Expr.t;
  word_size : int;
  builder_fn : ('reg, 'comm, 'rel, 'a) Builder_fn.t;
}
[@@deriving fields]

let create ?(len = 3) reg_m comm_m ~word_size ~builder_fn =
  let open Arg in
  {
    comm = None;
    args = Array.create ~len Uninit;
    ind = 0;
    ty = [];
    reg_m;
    comm_m;
    word_size;
    builder_fn;
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

let validate_arg (type reg) st ty arg =
  let open Arg in
  let open Isa in
  let module R = (val st.reg_m : Isa.Register.S with type t = reg) in
  match (ty, arg) with
  | Type.Reg n, Reg reg ->
      if R.bit_size reg <> n then failwith "Unexpected register length"
  | Type.Word n, Rel rel ->
      let bs = rel_bs st rel in
      if bs >= n then failwith "Incorrect bit length"
  | Type.Base_offset (off_size, base_size), Base_offset (off, base) ->
      let off_bs = rel_bs st off in
      let base_bs = R.bit_size base in
      if base_bs >= base_size || off_bs >= off_size then
        failwith "Something wrong with bit sizes"
  | Type.String, StringLiteral _ -> ()
  | _, _ -> failwith "Terribly wrong"

let add_arg st arg =
  match st.ty with
  | [] -> failwith "The comm is not expecting more arguments"
  | ty :: rst ->
      validate_arg st ty arg;
      unsafe_add_arg st arg rst

let add_register st reg = add_arg st (Reg reg)
let add_rel st rel = add_arg st (Rel rel)
let add_base_offset st base off = add_arg st (Base_offset (off, base))
let add_string st str = add_arg st (StringLiteral str)

let start (type comm) st comm =
  let module C = (val st.comm_m : Isa.Expr.S with type t = comm) in
  st.ty <- C.arg_type comm;
  st.ind <- 0;
  st.comm <- Some comm

let build st = st.builder_fn (Option.value_exn st.comm) st.args
