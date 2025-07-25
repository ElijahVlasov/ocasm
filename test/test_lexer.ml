open Base
open Ocasm_lexer
open Lexer

module Mock_token = Token.MkToken (struct
  type t = unit

  let to_string _ = ""
  let equal _ _ = true
end)

let token_fmt fmt token =
  Stdlib.Format.fprintf fmt "'%s'" (Mock_token.to_string token)

module MockT : Isa_token.S with type t = unit = struct
  type t = unit

  let directive _ = None
  let name _ = None
  let reserved _ = None
end

let token = Alcotest.testable token_fmt Mock_token.equal

let test_single_token content expected () =
  let module I = Ocasm_lexer.Input.StringInput in
  let input = I.create ~content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer = Lexer.create (module MockT) (module I) inp in
      let got = Lexer.next_token lexer |> Option.value_exn in
      Alcotest.check token "Tokens don't coincide" expected got)

let test_multiple_tokens content expected () =
  let module I = Ocasm_lexer.Input.StringInput in
  let input = I.create ~content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer = Lexer.create (module MockT) (module I) inp in
      let got = Lexer.to_list lexer |> Option.value_exn in
      Alcotest.check (Alcotest.list token) "Tokens don't coincide" expected got)

let tests_single_token =
  let open Token in
  [
    ("Curly bracket", `Quick, test_single_token "{}" LCurly);
    ( "Curly bracket with multiline comments",
      `Quick,
      test_single_token "/* sdagdgsa \n \n */ /* */{}" White_space );
    ("Binary", `Quick, test_single_token "0b1010" (Bin [| 0b1010L |]));
    ( "Binary long wonky length (255)",
      `Quick,
      test_single_token
        "0b101011000100110100101110110100110100101101010011010010110101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010"
        (Bin
           [|
             0b0110100101101001101001011010010110100110100101101001101001011010L;
             0b0110100110100101101001011010011010010110100110100101101001011010L;
             0b1010010110100101101001101001011010011010010110100101101001101001L;
             0b0101011000100110100101110110100110100101101010011010010110101001L;
           |]) );
    ( "Binary long normal length (256)",
      `Quick,
      test_single_token
        "0b1101011000100110100101110110100110100101101010011010010110101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010"
        (Bin
           [|
             0b0110100101101001101001011010010110100110100101101001101001011010L;
             0b0110100110100101101001011010011010010110100110100101101001011010L;
             0b1010010110100101101001101001011010011010010110100101101001101001L;
             0b1101011000100110100101110110100110100101101010011010010110101001L;
           |]) );
    ("Octal", `Quick, test_single_token "0000242455" (Oct [| 0o000242455L |]));
    ("Unicode", `Quick, test_single_token "🦜𱍫ퟲ" (Name "🦜𱍫ퟲ"));
    ("Ascii+unicode", `Quick, test_single_token "aaaa🦜𱍫ퟲ" (Name "aaaa🦜𱍫ퟲ"));
    ( "Ascii+unicode string literal",
      `Quick,
      test_single_token "\"aaaa🦜𱍫ퟲ\"" (String_literal "aaaa🦜𱍫ퟲ") );
    ( "Escaped string literal",
      `Quick,
      test_single_token "\"\\xaa🦜𱍫ퟲ \\x60\\a\\n\\r\""
        (String_literal
           "\170\240\159\166\156\240\177\141\171\237\159\178 `\007\n\r") )
    (* ("No leading zeros: 0", `Quick, test_single_token "0" (Dec [ 0L ])); *)
    (* ("No leading zeros: 50", `Quick, test_single_token "50" (Dec [ 50L ])); *);
  ]

let tests_multiple_tokens =
  let open Token in
  [
    ("Curly brackets", `Quick, test_multiple_tokens "{}" [ LCurly; RCurly; Eof ]);
    ( "Three \\n's",
      `Quick,
      test_multiple_tokens "\n \n\n" [ Eol; White_space; Eol; Eol; Eof ] );
    ( "Brackets and single line comment",
      `Quick,
      test_multiple_tokens "() # a single line comment\n()"
        [ LBracket; RBracket; White_space; Eol; LBracket; RBracket; Eof ] );
    ( "4 Names",
      `Quick,
      test_multiple_tokens "name1    name2\t \tna_____me3\n    \t....na..me.4"
        [
          Name "name1";
          White_space;
          Name "name2";
          White_space;
          Name "na_____me3";
          Eol;
          White_space;
          Name "....na..me.4";
          Eof;
        ] );
  ]

let suite = List.concat [ tests_single_token; tests_multiple_tokens ]
