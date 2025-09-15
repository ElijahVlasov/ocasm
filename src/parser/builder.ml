open! Import
open Isa

module Mk (Reg : Register.S) (Reloc_data : T.T) = struct
  type ('comm, 'a) t = {
    mutable comm : 'comm option;
    args : (Reg.t, Reloc_data.t) Argument.t array;
    mutable ind : int;
    mutable ty : Isa.Type.t list;
    comm_m : 'comm Isa.Expr.t;
    word_size : int;
    builder_fn : (Reg.t, 'comm, Reloc_data.t, 'a) Builder_fn.t;
  }
  [@@deriving fields]

  open Result
  open Result.Let_syntax

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

  let validate_arg st ty arg =
    let open Argument in
    let open Diagnostics.Error in
    let open Isa in
    match (ty, arg) with
    | Type.Reg n, Reg reg ->
        ok_if_true
          (Reg.bit_size reg = n)
          ~error:(Wrong_register_length (Reg.bit_size reg, n))
    | Type.Word n, Rel rel ->
        let bs = rel_bs st rel in
        ok_if_true (bs <= n) ~error:(Wrong_word_length (bs, n))
    | Type.Base_offset (off_size, base_size), Base_offset (off, base) ->
        let off_bs = rel_bs st off in
        let base_bs = Reg.bit_size base in
        let%bind () =
          ok_if_true (base_bs <= base_size)
            ~error:(Wrong_base_length (base_size, base_bs))
        in
        ok_if_true (off_bs <= off_size)
          ~error:(Wrong_offset_length (off_size, off_bs))
    | Type.String, StringLiteral _ -> return ()
    | _, _ -> Panic.unreachable ~msg:"Terribly wrong" ()

  let add_arg st arg =
    let open Diagnostics.Error in
    match st.ty with
    | [] -> fail @@ Too_many_args (Array.length st.args)
    | ty :: rst ->
        let%bind () = validate_arg st ty arg in
        return @@ unsafe_add_arg st arg rst

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

  let create ?(len = 3) comm_m ~word_size ~builder_fn =
    let open Argument in
    {
      comm = None;
      args = Array.create ~len Uninit;
      ind = 0;
      ty = [];
      comm_m;
      word_size;
      builder_fn;
    }
end
