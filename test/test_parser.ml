open! Import

let tokenize content =
  let open Mock_isa in
  let module I = Ocasm_lexer.Input.StringInput in
  let input = I.create content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let open Diagnostics_handler in
      let handler = Kitchen_sink_handler.create () in
      let lexer =
        Ocasm_lexer.create
          (module Mock_token)
          (module I)
          inp
          (module Kitchen_sink_handler)
          handler
      in
      Ocasm_lexer.to_seq lexer)

let test_parser input expected () =
  let open Mock_isa in
  let input = tokenize input in
  let parser =
    Ocasm_parser.create
      (module Mock_register)
      (module Mock_dir)
      (module Mock_opcode)
      (module Mock_reserved)
      ~word_size:32
      ~build_instruction:(fun
          opcode (args : (_, Mock_reserved.t) Argument.t array) ->
        let arg1 = Argument.unwrap_reg_exn (Array.unsafe_get args 0) in
        let arg2 = Argument.unwrap_reg_exn (Array.unsafe_get args 1) in
        let open Mock_opcode in
        match opcode with
        | Opcode1 -> Mock_instruction.Opcode1 (arg1, arg2)
        | Opcode2 -> Mock_instruction.Opcode2 (arg1, arg2))
      ~build_directive:(fun dir args ->
        let arg1 =
          Argument.unwrap_string_literal_exn (Array.unsafe_get args 0)
        in
        let open Mock_dir in
        match dir with
        | Dir1 -> Mock_directive.Dir1 arg1
        | Dir2 -> Mock_directive.Dir2 arg1)
      ~build_reserved:(fun (_ : Mock_reserved.t) _ -> Panic.unreachable ())
      input
  in
  let got =
    Ocasm_parser.to_list parser
    |> List.map ~f:(Option.value_or_thunk ~default:(fun () -> failwith ""))
  in
  Alcotest.check
    (Alcotest.list
    @@ Testable.command Testable.mock_instruction Testable.mock_directive)
    "" got expected

let parser_test_case name code parsed =
  test_case name `Quick (test_parser code parsed)

let suite =
  let open Command in
  let open Mock_isa in
  let open Mock_instruction in
  let open Mock_register in
  [
    parser_test_case "One opcode" "opcode1 reg1, reg2"
      [ instruction @@ Opcode1 (Reg1, Reg2) ];
    parser_test_case "Two opcodes"
      "    opcode1 reg2, reg1\n\n\n\t opcode2 reg2, reg2"
      [
        instruction @@ Opcode1 (Reg2, Reg1); instruction @@ Opcode2 (Reg2, Reg2);
      ];
    parser_test_case "Opcode, Label, Opcode, Label"
      " opcode2 reg1,       reg2\n\
       label1:\n\
       \topcode1 reg2,\t\t\t\t\t\t reg2\n\
       label2:"
      [
        instruction @@ Opcode2 (Reg1, Reg2);
        label "label1";
        instruction @@ Opcode1 (Reg2, Reg2);
        label "label2";
      ];
  ]
