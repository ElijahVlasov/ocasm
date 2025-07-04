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
      let rec consume lexer =
        let ( = ) = Lexer.Token.equal in
        let next = Lexer.next_token lexer in
        if next = Lexer.Token.End_of_file then [ Lexer.Token.End_of_file ]
        else next :: consume lexer
      in
      let lexer = Lexer.create (module I) inp in
      let got = consume lexer in
      Alcotest.check (Alcotest.list token) "Tokens don't coincide" expected got)

let tests_single_token =
  let open Lexer.Token in
  [
    ("Curly bracket", `Quick, test_single_token "{}" LCurly);
    ( "Curly bracket with multiline comments",
      `Quick,
      test_single_token "/* sdagdgsa \n \n */ /* */{}" LCurly );
  ]

let tests_multiple_tokens =
  let open Lexer.Token in
  [
    ( "Curly brackets",
      `Quick,
      test_multiple_tokens "{}" [ LCurly; RCurly; End_of_file ] );
  ]

let () =
  Alcotest.run "Lexer tests"
    [
      ("Single token", tests_single_token);
      ("Multiple tokens", tests_multiple_tokens);
    ]
