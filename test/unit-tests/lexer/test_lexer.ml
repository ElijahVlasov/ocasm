open! Import

let suite =
  [
    ("Input", Test_lexer_input.suite);
    ("Single token", Test_single_token.suite);
    ("Multiple tokens", Test_multiple_tokens.suite);
    ("Diagnostics", Test_diagnostics.suite);
  ]

let () = Alcotest.run "Lexer" suite
