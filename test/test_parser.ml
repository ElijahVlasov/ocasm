open! Import

let test_first () =
  let open Mock_isa in
  let open Mock_register in
  let open Mock_opcode in
  let open Token in
  let input =
    [
      Isa_specific (Isa.Token.Opcode Opcode1);
      White_space;
      White_space;
      White_space;
      White_space;
      White_space;
      White_space;
      Isa_specific (Isa.Token.Reg Reg1);
      Comma;
      Isa_specific (Isa.Token.Reg Reg2);
      Eof;
    ]
    |> List.map ~f:(fun tok ->
           Option.some
             ( tok,
               let open Token_info in
               {
                 starts = Location.create 1 1;
                 ends = Location.create 1 1;
                 string = (fun () -> "");
               } ))
    |> Sequence.of_list
  in
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
  Alcotest.check
    (Alcotest.option
    @@ Testable.command Testable.mock_instruction Testable.mock_directive)
    "" (Ocasm_parser.next parser)
    (Mock_instruction.Opcode1 (Mock_register.Reg1, Mock_register.Reg2)
    |> Command.instruction |> Option.some)

let suite = [ test_case "First" `Quick test_first ]
