open! Import
open Parser_dsl
module Argument = Argument
module Builder_fn = Builder.Builder_fn

type ('reg, 'dir, 'opcode, 'res, 'rel, 'ast) t = {
  dsl : ('reg, 'dir, 'opcode, 'res, 'rel, 'ast) Parser_dsl.t;
  build_label : string -> 'ast;
}

let label st name =
  (match peek_non_whitespace st.dsl with Eol -> skip st.dsl | _ -> ());
  st.build_label name

let must_be_label st name =
  let open Token in
  match next_non_whitespace st.dsl with
  | Colon -> label st name
  | _ -> failwith "Unexpected token"

let parse_arg st bldr next =
  let open Token in
  let open Isa.Token in
  (match next with
  | Isa_specific (Reg reg) -> Parser_dsl.add_register st.dsl bldr reg
  | Name name -> Parser_dsl.add_rel st.dsl bldr (Relocatable.Name name)
  | Dec x -> failwith "lol"
  | White_space -> failwith "Whitespace"
  | _ -> failwith "Wrong tok");
  let next = next_non_whitespace st.dsl in
  match next with
  | Comma -> false
  | Eol | Eof -> true
  | _ -> failwith "Wrong token"

let rec parse_args st bldr next =
  let is_finished = parse_arg st bldr next in
  if is_finished then build st.dsl bldr
  else
    let next = next_non_whitespace st.dsl in
    parse_args st bldr next

let build_command st next with_builder =
  with_builder @@ fun bldr -> parse_args st bldr next

let maybe_command st with_builder =
  let open Token_info in
  let token_info = last_token_info st.dsl in
  let name = token_info.string () in
  let next = next_non_whitespace st.dsl in
  match next with
  | Colon -> label st name
  | _ -> build_command st next with_builder

let entry_point st token =
  let open Token in
  let open Isa.Token in
  try
    Option.some
    @@
    match token with
    | Name name -> must_be_label st name
    | Isa_specific (Opcode opcode) ->
        maybe_command st (with_opcode_builder st.dsl opcode)
    | Isa_specific (Dir dir) -> maybe_command st (with_dir_builder st.dsl dir)
    | tok -> failwith "Unexpected token"
  with Token_reader.Lexer_error -> None

let next st = next_non_whitespace st.dsl |> entry_point st

let create ?(path = Path.empty) reg_m dir_m opcode_m res_m ~word_size
    ~build_instruction ~build_directive ~build_reserved ~build_label toks =
  {
    dsl =
      create ~path reg_m dir_m opcode_m res_m ~word_size ~build_instruction
        ~build_directive ~build_reserved toks;
    build_label;
  }
