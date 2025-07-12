open Ocasm_parser

let token_fmt fmt token =
  Stdlib.Format.fprintf fmt "'%s'" (Token.to_string token)

let token = Alcotest.testable token_fmt Token.equal

let test_single_token content expected () =
  let module I = Ocasm_parser.Input.StringInput in
  let input = I.create ~content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer = Lexer.create (module I) inp in
      let got = Lexer.next_token lexer in
      Alcotest.check token "Tokens don't coincide" expected got)

let test_multiple_tokens content expected () =
  let module I = Ocasm_parser.Input.StringInput in
  let input = I.create ~content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer = Lexer.create (module I) inp in
      let got = Lexer.to_list lexer in
      Alcotest.check (Alcotest.list token) "Tokens don't coincide" expected got)

let tests_single_token =
  let open Token in
  [
    ("Curly bracket", `Quick, test_single_token "{}" LCurly);
    ( "Curly bracket with multiline comments",
      `Quick,
      test_single_token "/* sdagdgsa \n \n */ /* */{}" LCurly );
    ( "Binary",
      `Quick,
      test_single_token "0b1010" (Bin (Array.of_list [ 0b1010L ])) );
    ( "Binary long wonky length (255)",
      `Quick,
      test_single_token
        "0b101011000100110100101110110100110100101101010011010010110101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010"
        (Bin
           (Array.of_list
              [
                0b0110100101101001101001011010010110100110100101101001101001011010L;
                0b0110100110100101101001011010011010010110100110100101101001011010L;
                0b1010010110100101101001101001011010011010010110100101101001101001L;
                0b0101011000100110100101110110100110100101101010011010010110101001L;
              ])) );
    ( "Binary long normal length (256)",
      `Quick,
      test_single_token
        "0b1101011000100110100101110110100110100101101010011010010110101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010"
        (Bin
           (Array.of_list
              [
                0b0110100101101001101001011010010110100110100101101001101001011010L;
                0b0110100110100101101001011010011010010110100110100101101001011010L;
                0b1010010110100101101001101001011010011010010110100101101001101001L;
                0b1101011000100110100101110110100110100101101010011010010110101001L;
              ])) );
    ( "Octal",
      `Quick,
      test_single_token "0000242455" (Oct (Array.of_list [ 0o000242455L ])) );
    (* ("No leading zeros: 0", `Quick, test_single_token "0" (Dec [ 0L ])); *)
    (* ("No leading zeros: 50", `Quick, test_single_token "50" (Dec [ 50L ])); *)
  ]

let tests_multiple_tokens =
  let open Token in
  [
    ("Curly brackets", `Quick, test_multiple_tokens "{}" [ LCurly; RCurly; Eof ]);
    ( "Three \\n's",
      `Quick,
      test_multiple_tokens "\n \n\n" [ Eol; Eol; Eol; Eof ] );
  ]

let () =
  Alcotest.run "Lexer tests"
    [
      ("Single token", tests_single_token);
      ("Multiple tokens", tests_multiple_tokens);
    ]
