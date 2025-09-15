open! Import
open Isa

module Mk
    (Opcode : Expr.S)
    (Direc : Expr.S)
    (Reserved : Expr.S)
    (Reg : Register.S)
    (Reloc_data : T.T) =
struct
  module Builder = Builder.Mk (Reg) (Reloc_data)

  type 'a result =
    ('a, (Diagnostics.Error.t, Diagnostics.Warning.t) Either.t) Result.t

  type 'out t = {
    path : Path.t;
    opcode_builder : (Opcode.t, 'out) Builder.t;
    dir_builder : (Direc.t, 'out) Builder.t;
    res_builder : (Reserved.t, Reloc_data.t Relocatable.t) Builder.t;
    token_reader :
      (Reg.t, Direc.t, Opcode.t, Reserved.t) Isa.Token.t Token_reader.t;
    dgn_printer : Diagnostics_printer.t;
  }
  [@@deriving fields]

  let with_opcode_builder st opcode f =
    Builder.start st.opcode_builder opcode;
    f st.opcode_builder

  let with_dir_builder st dir f =
    Builder.start st.dir_builder dir;
    f st.dir_builder

  let map_error = Result.map_error ~f:(fun err -> Either.First err)
  let add_register _st bldr reg = map_error @@ Builder.add_register bldr reg
  let add_rel _st bldr rel = map_error @@ Builder.add_rel bldr rel
  let add_string _st bldr str = map_error @@ Builder.add_string bldr str

  let add_base_offset _st bldr base off =
    map_error @@ Builder.add_base_offset bldr base off

  let build _st bldr = Builder.build bldr

  let next st =
    let t = Token_reader.next st.token_reader in
    t

  let peek st = Token_reader.peek st.token_reader

  let skip st =
    let _ = Token_reader.next st.token_reader in
    ()

  let last_token_info st = Token_reader.last_token_info st.token_reader

  let rec next_non_whitespace st =
    match next st with White_space -> next_non_whitespace st | tok -> tok

  let rec peek_non_whitespace st =
    match peek st with
    | White_space ->
        skip st;
        peek_non_whitespace st
    | tok -> tok

  let rec skip_whitespaces_and_newlines st =
    match peek st with
    | White_space | Eol ->
        skip st;
        skip_whitespaces_and_newlines st
    | _ -> ()

  let pos st =
    let open Token_info in
    let token_info = Token_reader.last_token_info st.token_reader in
    (token_info.starts, token_info.ends)

  let emit_diagnostic_message st err_or_warn =
    let open Either in
    let open Diagnostics_message in
    let id, msg, ty =
      match err_or_warn with
      | First err ->
          ( Diagnostics.Error.id err,
            Diagnostics.Error.show err,
            Diagnostics_type.Error )
      | Second warn ->
          ( Diagnostics.Warning.id warn,
            Diagnostics.Warning.show warn,
            Diagnostics_type.Warning )
    in
    let starts, ends = pos st in
    let dgn_msg = { id; ty; msg; starts; ends; file = path st; ctx = "" } in
    Diagnostics_printer.emit st.dgn_printer dgn_msg err_or_warn

  let error st err =
    let open Result.Let_syntax in
    let err = Either.First err in
    (* We now that Diagnostics_printer will return Error anyway when we pass an
       error. *)
    let%bind () = emit_diagnostic_message st err in
    Error err

  let warning st warn = emit_diagnostic_message st (Either.Second warn)

  let create ?(path = Path.empty) ~word_size ~build_instruction ~build_directive
      ~build_reserved toks dgn_printer =
    {
      path;
      opcode_builder =
        Builder.create (module Opcode) ~word_size ~builder_fn:build_instruction;
      dir_builder =
        Builder.create (module Direc) ~word_size ~builder_fn:build_directive;
      res_builder =
        Builder.create (module Reserved) ~word_size ~builder_fn:build_reserved;
      token_reader = Token_reader.create toks;
      dgn_printer;
    }
end
