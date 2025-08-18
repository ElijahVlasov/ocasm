open! Import
open Option.Let_syntax
open Parser_dsl

type ('reg, 'dir, 'opcode, 'res, 'rel, 'ast) t = {
  dsl : ('reg, 'dir, 'opcode, 'res, 'rel, 'ast) Parser_dsl.t;
  build_label : string -> 'ast;
}

let label st name =
  (match next_non_whitespace st.dsl with
  | Some (Eol, _) -> ()
  | _ -> roll_back st.dsl);
  return @@ st.build_label name

let must_be_label st name =
  let open Token in
  match%map next_non_whitespace st.dsl with
  | Colon, _ -> label st name
  | _ -> failwith "Unexpected token"

let parse_arg st bldr next next_info =
  let open Token in
  let open Isa.Token in
  (match next with
  | Isa_specific (Reg reg) -> Parser_dsl.add_register st.dsl bldr reg
  | Name name -> Parser_dsl.add_rel st.dsl bldr (Relocatable.Name name)
  | Dec x -> failwith "lol"
  | _ -> failwith "Wrong token");
  let%map next, _ = next_non_whitespace st.dsl in
  match next with
  | Comma -> false
  | Eol | Eof -> true
  | _ -> failwith "Wrong token"

let rec parse_args st bldr next next_info =
  let%bind is_finished = parse_arg st bldr next next_info in
  if is_finished then return @@ build st.dsl bldr
  else
    let%bind next, next_info = next_non_whitespace st.dsl in
    parse_args st bldr next next_info

let must_be_opcode st opcode next next_info =
  with_opcode_builder st.dsl opcode @@ fun bldr ->
  parse_args st bldr next next_info

let may_be_opcode st opcode token_info =
  let open Token in
  let open Token_info in
  let name = token_info.string () in
  let%map next, next_info = next_non_whitespace st.dsl in
  match next with
  | Colon -> label st name
  | _ -> must_be_opcode st opcode next next_info

let entry_point st (token, token_info) =
  let open Token in
  let open Isa.Token in
  match token with
  | Name name -> must_be_label st name
  | Isa_specific (Opcode t) -> may_be_opcode st t token_info
  | tok -> failwith "Unexpected token"

let next st = next_non_whitespace st.dsl >>= entry_point st

let create ?(path = Path.empty) reg_m dir_m opcode_m res_m ~word_size
    ~build_instruction ~build_directive ~build_reserved ~build_label toks =
  {
    dsl =
      create ~path reg_m dir_m opcode_m res_m ~word_size ~build_instruction
        ~build_directive ~build_reserved toks;
    build_label;
  }
