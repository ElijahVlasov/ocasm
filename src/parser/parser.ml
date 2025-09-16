open! Import
module Argument = Argument
module Builder_fn = Builder_fn

module Mk (I : sig
  module Reg : Isa.Register.S

  type reloc_data
  type structured_instruction

  module Opcode : sig
    include Isa.Expr.S

    val build : (Reg.t, t, reloc_data, structured_instruction) Builder_fn.t
  end

  type structured_directive

  module Directive : sig
    include Isa.Expr.S

    val build : (Reg.t, t, reloc_data, structured_directive) Builder_fn.t
  end

  module Reserved : sig
    include Isa.Expr.S

    val build : (Reg.t, t, reloc_data, reloc_data Relocatable.t) Builder_fn.t
  end
end) =
struct
  module Parser_dsl = Parser_dsl.Mk (I)
  open Parser_dsl

  type t = { dsl : Parser_dsl.t; mutable no_errors : bool } [@@deriving fields]

  let label st name =
    (match peek_non_whitespace st.dsl with Eol -> skip st.dsl | _ -> ());
    Command.Label name

  open Result.Let_syntax

  let must_be_label st name =
    let open Token in
    match next_non_whitespace st.dsl with
    | Colon -> return @@ label st name
    | _ -> error st.dsl @@ Not_a_command name

  let parse_register st =
    let open Token in
    let open Isa.Token in
    match next_non_whitespace st.dsl with
    | Isa_specific (Reg reg) -> return reg
    | _ -> error st.dsl @@ Expected_register

  let maybe_offset st bldr off =
    let open Token in
    match peek_non_whitespace st.dsl with
    | LBracket -> (
        skip st.dsl;
        let%bind reg = parse_register st in
        match next_non_whitespace st.dsl with
        | RBracket -> Parser_dsl.add_base_offset st.dsl bldr reg off
        | _ -> error st.dsl @@ Expected RBracket)
    | _ -> Parser_dsl.add_rel st.dsl bldr off

  let parse_arg st bldr next =
    let open Token in
    let open Isa.Token in
    let%bind x =
      match next with
      | Isa_specific (Reg reg) -> Parser_dsl.add_register st.dsl bldr reg
      | Name name -> Relocatable.Name name |> maybe_offset st bldr
      | Dec x | Bin x | Hex x | Oct x ->
          Relocatable.of_big_integer x |> maybe_offset st bldr
      | String_literal str -> Parser_dsl.add_string st.dsl bldr str
      | White_space -> Panic.unreachable ~msg:"Whitespace" ()
      | t ->
          failwith
            (Stdlib.Format.sprintf "%a" (fun _ -> Token.show (fun _ _ -> ())) t)
    in
    let next = next_non_whitespace st.dsl in
    match next with
    | Comma -> return false
    | Eol | Eof -> return true
    | _ -> error st.dsl @@ Expected Comma

  let rec parse_args st bldr next =
    let%bind is_finished = parse_arg st bldr next in
    if is_finished then return @@ build st.dsl bldr
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
    | Colon -> return @@ label st name
    | _ -> build_command st next with_builder

  let next_aux st =
    let open Token in
    let open Isa.Token in
    skip_whitespaces_and_newlines st.dsl;
    match next_non_whitespace st.dsl with
    | Name name -> must_be_label st name
    | Isa_specific (Opcode opcode) ->
        maybe_command st (with_opcode_builder st.dsl opcode)
    | Isa_specific (Dir dir) -> maybe_command st (with_dir_builder st.dsl dir)
    | Eof -> return Command.Eof
    | tok -> failwith "Unexpected token"

  (* TODO: implement this. *)
  let recover_aux st =
    let open Either in
    let open Diagnostics.Error in
    let open Diagnostics.Warning in
    function
    | Second Test -> Panic.unimplemented ()
    | First (Not_a_command name) -> Panic.unimplemented ()
    | First Expected_register -> Panic.unimplemented ()
    | First (Expected tok) -> Panic.unimplemented ()
    | First (Wrong_register_length (expected, got)) -> Panic.unimplemented ()
    | First (Wrong_word_length (expected, got)) -> Panic.unimplemented ()
    | First (Wrong_base_length (expected, got)) -> Panic.unimplemented ()
    | First (Wrong_offset_length (expected, got)) -> Panic.unimplemented ()
    | First (Too_many_args n) -> Panic.unimplemented () (* added *)

  let rec recover st err =
    match recover_aux st err with Ok () -> () | Error err -> recover st err

  let rec next st =
    match next_aux st with
    | Ok next -> next
    | Error err ->
        st.no_errors <- false;
        recover st err;
        next st

  let create ?(path = Path.empty) ~word_size ~build_instruction ~build_directive
      ~build_reserved toks dgn_printer =
    let build_instruction x arg =
      Command.instruction @@ build_instruction x arg
    in
    let build_directive x arg = Command.directive @@ build_directive x arg in
    {
      dsl =
        create ~path ~word_size ~build_instruction ~build_directive
          ~build_reserved toks dgn_printer;
      no_errors = true;
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
end
