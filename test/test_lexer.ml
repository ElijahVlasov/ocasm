open Ocasm_parser

let token_fmt fmt token =
  Stdlib.Format.fprintf fmt "'%s'" (Token.to_string token)

let common_token = Alcotest.testable token_fmt Token.equal

module MockT : Lexer.T with type token = unit = struct
  type token = unit

  let directive _ = None
  let name _ = None
  let reserved _ = None
end

let wrapped_token_fmt fmt token =
  let open Lexer in
  match token with
  | Isa_specific _ -> Stdlib.Format.fprintf fmt ""
  | Common t -> token_fmt fmt t

let wrapped_token =
  Alcotest.testable wrapped_token_fmt (Lexer.equal_token Unit.equal)

let test_single_token content expected () =
  let module I = Ocasm_parser.Input.StringInput in
  let input = I.create ~content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer = Lexer.create (module MockT) (module I) inp in
      let got = Lexer.next_token lexer in
      Alcotest.check wrapped_token "Tokens don't coincide" expected got)

let test_multiple_tokens content expected () =
  let module I = Ocasm_parser.Input.StringInput in
  let input = I.create ~content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer = Lexer.create (module MockT) (module I) inp in
      let got = Lexer.to_list lexer in
      Alcotest.check
        (Alcotest.list wrapped_token)
        "Tokens don't coincide" expected got)

let tests_single_token =
  let open Token in
  [
    ("Curly bracket", `Quick, test_single_token "{}" (Common LCurly));
    ( "Curly bracket with multiline comments",
      `Quick,
      test_single_token "/* sdagdgsa \n \n */ /* */{}" (Common White_space) );
    ( "Binary",
      `Quick,
      test_single_token "0b1010" (Common (Bin (Array.of_list [ 0b1010L ]))) );
    ( "Binary long wonky length (255)",
      `Quick,
      test_single_token
        "0b101011000100110100101110110100110100101101010011010010110101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010"
        (Common
           (Bin
              (Array.of_list
                 [
                   0b0110100101101001101001011010010110100110100101101001101001011010L;
                   0b0110100110100101101001011010011010010110100110100101101001011010L;
                   0b1010010110100101101001101001011010011010010110100101101001101001L;
                   0b0101011000100110100101110110100110100101101010011010010110101001L;
                 ]))) );
    ( "Binary long normal length (256)",
      `Quick,
      test_single_token
        "0b1101011000100110100101110110100110100101101010011010010110101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010"
        (Common
           (Bin
              (Array.of_list
                 [
                   0b0110100101101001101001011010010110100110100101101001101001011010L;
                   0b0110100110100101101001011010011010010110100110100101101001011010L;
                   0b1010010110100101101001101001011010011010010110100101101001101001L;
                   0b1101011000100110100101110110100110100101101010011010010110101001L;
                 ]))) );
    ( "Octal",
      `Quick,
      test_single_token "0000242455"
        (Common (Oct (Array.of_list [ 0o000242455L ]))) );
    (* ("No leading zeros: 0", `Quick, test_single_token "0" (Dec [ 0L ])); *)
    (* ("No leading zeros: 50", `Quick, test_single_token "50" (Dec [ 50L ])); *)
  ]

let tests_multiple_tokens =
  let open Token in
  [
    ( "Curly brackets",
      `Quick,
      test_multiple_tokens "{}" [ Common LCurly; Common RCurly; Common Eof ] );
    ( "Three \\n's",
      `Quick,
      test_multiple_tokens "\n \n\n"
        [ Common Eol; Common White_space; Common Eol; Common Eol; Common Eof ]
    );
    ( "4 Names",
      `Quick,
      test_multiple_tokens "name1    name2\t \tna_____me3\n    \t....na..me.4"
        [
          Common (Name "name1");
          Common White_space;
          Common (Name "name2");
          Common White_space;
          Common (Name "na_____me3");
          Common Eol;
          Common White_space;
          Common (Name "....na..me.4");
          Common Eof;
        ] );
  ]

let suite = List.concat [ tests_single_token; tests_multiple_tokens ]
