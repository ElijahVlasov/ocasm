open Ocasm_parser

let token_fmt fmt token = Format.fprintf fmt "%s" (Lexer.Token.show token)
let token = Alcotest.testable token_fmt Lexer.Token.equal

let test_one_token content expected () =
  let module I = Ocasm_parser.Input.StringInput in
  let input = I.create ~content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer = Lexer.create (module I) inp in
      let got = Lexer.next_token lexer in
      Alcotest.check token "Tokens don't coincide" expected got)

let () =
  let open Lexer.Token in
  Alcotest.run "Lexer tests"
    [
      ( "Single token",
        [
          ("Curly bracket", `Quick, test_one_token "{}" LCurly);
          ( "Curly bracket with multiline comments",
            `Quick,
            test_one_token "/* sdagdgsa \n \n */ /* */{}" LCurly );
        ] );
    ]
