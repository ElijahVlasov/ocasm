open Ocasm_parser

let token_fmt fmt token =
  Stdlib.Format.fprintf fmt "%s" (Lexer.Token.show token)

let token = Alcotest.testable token_fmt Lexer.Token.equal

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
  let open Lexer.Token in
  [
    ("Curly bracket", `Quick, test_single_token "{}" LCurly);
    ( "Curly bracket with multiline comments",
      `Quick,
      test_single_token "/* sdagdgsa \n \n */ /* */{}" LCurly );
    ("Binary", `Quick, test_single_token "0b1010" (Bin "0b1010"));
    ("Octal", `Quick, test_single_token "0000242455" (Oct "0000242455"));
    ("No leading zeros: 0", `Quick, test_single_token "0" (Dec "0"));
    ("No leading zeros: 50", `Quick, test_single_token "50" (Dec "50"));
  ]

let tests_multiple_tokens =
  let open Lexer.Token in
  [
    ( "Curly brackets",
      `Quick,
      test_multiple_tokens "{}" [ LCurly; RCurly; End_of_file ] );
    ( "Three \\n's",
      `Quick,
      test_multiple_tokens "\n \n\n"
        [ End_of_line; End_of_line; End_of_line; End_of_file ] );
  ]

let () =
  Alcotest.run "Lexer tests"
    [
      ("Single token", tests_single_token);
      ("Multiple tokens", tests_multiple_tokens);
    ]
