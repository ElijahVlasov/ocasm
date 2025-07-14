let suite =
  [
    ("Bfd test suite", Test_bfd.suite);
    ("Fence type test suite", Test_fence_type.suite);
    ("Immediate type test suite", Test_immediate.suite);
    ("Input suite", Test_parser_input.suite);
    ("Instruction test suite", Test_instruction.suite);
    ("Lexer test suite", Test_lexer.suite);
    ("Parser code snippet suite", Test_parser_code_snippets.suite);
    ("Single instruction suite", Test_single_instruction_parsing.suite);
  ]

let () = Alcotest.run ~json:true "ocasm" suite
