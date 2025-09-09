open! Import
open Parser_dsl
module Argument = Argument
module Builder_fn = Builder.Builder_fn

type ('reg, 'dir, 'opcode, 'res, 'rel, 'instr, 'dir_ast) t = {
  dsl :
    ('reg, 'dir, 'opcode, 'res, 'rel, ('instr, 'dir_ast) Command.t) Parser_dsl.t;
}

let label st name =
  (match peek_non_whitespace st.dsl with Eol -> skip st.dsl | _ -> ());
  Command.Label name

let must_be_label st name =
  let open Token in
  match next_non_whitespace st.dsl with
  | Colon -> label st name
  | _ -> failwith "Unexpected token"

let parse_register st =
  let open Token in
  let open Isa.Token in
  match next_non_whitespace st.dsl with
  | Isa_specific (Reg reg) -> reg
  | _ -> failwith "Unexpected token"

let maybe_offset st bldr off =
  let open Token in
  match peek_non_whitespace st.dsl with
  | LBracket -> (
      skip st.dsl;
      let reg = parse_register st in
      match next_non_whitespace st.dsl with
      | RBracket -> Parser_dsl.add_base_offset st.dsl bldr reg off
      | _ -> failwith "Expected a closing bracket ")
  | _ -> Parser_dsl.add_rel st.dsl bldr off

let parse_arg st bldr next =
  let open Token in
  let open Isa.Token in
  (match next with
  | Isa_specific (Reg reg) -> Parser_dsl.add_register st.dsl bldr reg
  | Name name -> Relocatable.Name name |> maybe_offset st bldr
  | Dec x | Bin x | Hex x | Oct x ->
      Relocatable.of_big_integer x |> maybe_offset st bldr
  | String_literal str -> Parser_dsl.add_string st.dsl bldr str
  | White_space -> Panic.unreachable ~msg:"Whitespace" ()
  | t ->
      failwith
        (Stdlib.Format.sprintf "%a" (fun _ -> Token.show (fun _ _ -> ())) t));
  let next = next_non_whitespace st.dsl in
  match next with
  | Comma -> false
  | Eol | Eof -> true
  | _ -> failwith "Wrong token at the end of an argument"

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
  let name = token_info.string in
  let next = next_non_whitespace st.dsl in
  match next with
  | Colon -> label st name
  | _ -> build_command st next with_builder

let entry_point st token =
  let open Token in
  let open Isa.Token in
  match token with
  | Name name -> must_be_label st name
  | Isa_specific (Opcode opcode) ->
      maybe_command st (with_opcode_builder st.dsl opcode)
  | Isa_specific (Dir dir) -> maybe_command st (with_dir_builder st.dsl dir)
  | Eof -> Command.Eof
  | tok -> failwith "Unexpected token"

let next st =
  skip_whitespaces_and_newlines st.dsl;
  next_non_whitespace st.dsl |> entry_point st

let create ?(path = Path.empty) reg_m dir_m opcode_m res_m ~word_size
    ~build_instruction ~build_directive ~build_reserved toks =
  let build_instruction x arg =
    Command.instruction @@ build_instruction x arg
  in
  let build_directive x arg = Command.directive @@ build_directive x arg in
  {
    dsl =
      create ~path reg_m dir_m opcode_m res_m ~word_size ~build_instruction
        ~build_directive ~build_reserved toks;
  }

let to_seq st =
  let open Sequence.Generator in
  let rec consume_output () =
    match next st with
    | Eof -> return ()
    | token -> yield token >>= consume_output
  in
  run @@ consume_output ()

let to_list st = Sequence.to_list (to_seq st)
