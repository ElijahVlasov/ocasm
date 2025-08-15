open! Import

type ('reg, 'dir, 'opcode, 'res, 'rel, 'out, 'tok) t = {
  path : Path.t;
  opcode_builder : ('reg, 'opcode, 'rel, 'out) Builder.t;
  dir_builder : ('reg, 'dir, 'rel, 'out) Builder.t;
  res_builder : ('reg, 'res, 'rel, 'out) Builder.t;
  mutable toks : ('tok Token.t * Token_info.t) option Sequence.t;
}
[@@deriving fields]

let with_opcode_builder st opcode f =
  Builder.start st.opcode_builder opcode;
  f st.opcode_builder

let with_dir_builder st dir f =
  Builder.start st.dir_builder dir;
  f st.dir_builder

let add_register _st bldr reg = Builder.add_register bldr reg
let add_rel _st bldr rel = Builder.add_rel bldr rel
let add_string _st bldr str = Builder.add_string bldr str
let add_base_offset _st bldr base off = Builder.add_base_offset bldr base off
let build _st bldr = Builder.build bldr

let next st =
  let open Token_info in
  match Sequence.next st.toks with
  | None ->
      Some
        ( Token.Eof,
          (* TODO: I wish we had Rust's Default trait. :) *)
          {
            starts = Location.create 1 1;
            ends = Location.create 1 1;
            string = (fun () -> "");
          } )
  | Some (next, tail) ->
      st.toks <- tail;
      next

let create ?(path = Path.empty) reg_m dir_m opcode_m res_m ~word_size
    ~build_instruction ~build_directive ~build_reserved toks =
  {
    path;
    opcode_builder =
      Builder.create reg_m opcode_m ~word_size ~builder_fn:build_instruction;
    dir_builder =
      Builder.create reg_m dir_m ~word_size ~builder_fn:build_directive;
    res_builder =
      Builder.create reg_m res_m ~word_size ~builder_fn:build_reserved;
    toks;
  }
