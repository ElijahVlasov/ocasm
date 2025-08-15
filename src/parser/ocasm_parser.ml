open! Import
open Option.Let_syntax
open Parser_dsl

type ('reg, 'dir, 'opcode, 'res, 'rel, 'ast) t = {
  dsl : ('reg, 'dir, 'opcode, 'res, 'rel, 'ast) Parser_dsl.t;
  build_label : string -> 'ast;
}

let must_be_label st name =
  let open Token in
  match%map next_non_whitespace st.dsl with
  | Colon, _ -> return @@ st.build_label name
  | _ -> failwith "Unexpected token"

let pre_label st (token, token_info) =
  let open Token in
  match token with
  | Name name -> must_be_label st name
  | tok -> failwith "Unexpected token"

let post_label st (token, token_info) =
  let open Token in
  match token with
  | Name _ -> failwith "Unexpected token"
  | tok -> failwith "Unexpected token"

let next st =
  next_non_whitespace st.dsl
  >>= choose_entry_point st.dsl ~pre_label:(pre_label st)
        ~post_label:(post_label st)

let create ?(path = Path.empty) reg_m dir_m opcode_m res_m ~word_size
    ~build_instruction ~build_directive ~build_reserved ~build_label toks =
  {
    dsl =
      create ~path reg_m dir_m opcode_m res_m ~word_size ~build_instruction
        ~build_directive ~build_reserved toks;
    build_label;
  }
