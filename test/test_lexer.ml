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
  let to_string _ = ""
  let equal _ _ = true
end

let token = Alcotest.testable token_fmt Mock_token.equal

let token_info_fmt fmt info =
  Stdlib.Format.fprintf fmt "%s" (Token_info.to_string info)

let token_info = Alcotest.testable token_info_fmt Token_info.equal

module Private = struct
  module Single_token_test : sig
    type t

    val mk : string -> string -> MockT.t Token.t -> t
    val to_test_case : t -> unit Alcotest.test_case
  end = struct
    type t = string * string * MockT.t Token.t

    let mk name content expected = (name, content, expected)

    let run content expected () =
      let module I = Ocasm_lexer.Input.StringInput in
      let input = I.create ~content in
      Input.with_input
        (module I)
        input
        ~f:(fun inp ->
          let lexer = Lexer.create (module MockT) (module I) inp in
          let got = Lexer.next_token lexer |> Option.value_exn |> fst in
          Alcotest.check token "Tokens don't coincide" expected got)

    let to_test_case (name, content, expected) =
      Alcotest.test_case name `Quick @@ run content expected
  end

  module Multiple_token_test : sig
    type t

    val mk : string -> string -> (MockT.t Token.t * Token_info.t) list -> t
    val to_test_case : t -> unit Alcotest.test_case
  end = struct
    type t = string * string * (MockT.t Token.t * Token_info.t) list

    let mk name content expected = (name, content, expected)

    let run content expected () =
      let module I = Ocasm_lexer.Input.StringInput in
      let input = I.create ~content in
      Input.with_input
        (module I)
        input
        ~f:(fun inp ->
          let lexer = Lexer.create (module MockT) (module I) inp in
          Lexer.to_list lexer |> Option.value_exn |> List.zip_exn expected
          |> List.iter ~f:(fun (expected, got) ->
                 Alcotest.check
                   (Alcotest.pair token token_info)
                   "Tokens don't coincide" expected got))

    let to_test_case (name, content, expected) =
      Alcotest.test_case name `Quick @@ run content expected
  end
end

open Private

let test_single_token =
  let open Token in
  let open Single_token_test in
  [
    mk "Binary"                       "0b1010"          (Bin [| 0b1010L |]);
    mk "Octal"                        "0000242455"      (Oct [| 0o000242455L |]);
    mk "Unicode"                      "🦜𱍫ퟲ"           (Name "🦜𱍫ퟲ");
    mk "Ascii+unicode"                "aaaa🦜𱍫ퟲ"       (Name "aaaa🦜𱍫ퟲ");
    mk "Ascii+unicode string literal" "\"aaaa🦜𱍫ퟲ\""   (String_literal "aaaa🦜𱍫ퟲ");
    mk "Curly bracket"                "{}"              LCurly;
    mk 
      "Escaped string literal" 
      "\"\\xaa🦜𱍫ퟲ \\x60\\a\\n\\r\""
      (String_literal
         "\170\240\159\166\156\240\177\141\171\237\159\178 `\007\n\r");
    mk 
      "Curly bracket with multiline comments" 
      "/* sdagdgsa \n \n */ /* */{}"
      White_space;
    mk 
      "Binary long wonky length (255)"
      "0b101011000100110100101110110100110100101101010011010010110101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010"
      (Bin
         [|
           0b0110100101101001101001011010010110100110100101101001101001011010L;
           0b0110100110100101101001011010011010010110100110100101101001011010L;
           0b1010010110100101101001101001011010011010010110100101101001101001L;
           0b0101011000100110100101110110100110100101101010011010010110101001L;
         |]);
    mk 
      "Binary long normal length (256)"
      "0b1101011000100110100101110110100110100101101010011010010110101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010"
      (Bin
         [|
           0b0110100101101001101001011010010110100110100101101001101001011010L;
           0b0110100110100101101001011010011010010110100110100101101001011010L;
           0b1010010110100101101001101001011010011010010110100101101001101001L;
           0b1101011000100110100101110110100110100101101010011010010110101001L;
         |]);
  ][@@ocamlformat "disable"]

let test_multiple_tokens =
  let open Token in
  let open Multiple_token_test in
  [
    mk 
      "Curly brackets" 
      "{}"
      [
        (LCurly, { starts = (1, 1); ends = (1, 2); string = (fun () -> "{") });
        (RCurly, { starts = (1, 2); ends = (1, 3); string = (fun () -> "}") });
        (Eof,    { starts = (1, 3); ends = (1, 4); string = (fun () -> "EOF") });
      ];
    mk 
      "Three \\n's" 
      "\n \n\n"
      [
        ( Eol, 
          { starts = (1, 1); ends = (2, 1); string = (fun () -> "\\n") } );
        ( White_space,
          { starts = (2, 1); ends = (2, 2); string = (fun () -> " ") } );
        ( Eol, 
          { starts = (2, 2); ends = (3, 1); string = (fun () -> "\\n") } );
        ( Eol, 
          { starts = (3, 1); ends = (4, 1); string = (fun () -> "\\n") } );
        ( Eof, 
          { starts = (4, 1); ends = (4, 2); string = (fun () -> "EOF") } );
      ];
    mk 
      "Brackets and single line comment" 
      "() # a single line comment\n()"
      [
        ( LBracket, 
          { starts = (1, 1);  ends = (1, 2);  string = (fun () -> "(") } );
        ( RBracket, 
          { starts = (1, 2);  ends = (1, 3);  string = (fun () -> ")") } );
        ( White_space,
          { starts = (1, 3);  ends = (1, 27); string = (fun () -> " ") } );
        ( Eol, 
          { starts = (1, 27); ends = (2, 1);  string = (fun () -> "\\n") } );
        ( LBracket, 
          { starts = (2, 1);  ends = (2, 2);  string = (fun () -> "(") } );
        ( RBracket, 
          { starts = (2, 2);  ends = (2, 3);  string = (fun () -> ")") } );
        ( Eof, 
          { starts = (2, 3);  ends = (2, 4);  string = (fun () -> "EOF") } );
      ];
    mk "4 Names" "name1    name2\t \tna_____me3\n    \t....na..me.4"
      [
        ( Name "name1",
          { starts = (1, 1);  ends = (1, 6);  string = (fun () -> "name1") } );
        ( White_space,
          { starts = (1, 6);  ends = (1, 10); string = (fun () -> " ") } );
        ( Name "name2",
          { starts = (1, 10); ends = (1, 15); string = (fun () -> "name2") } );
        ( White_space,
          { starts = (1, 15); ends = (1, 18); string = (fun () -> " ") } );
        ( Name "na_____me3",
          {
            starts = (1, 18); ends = (1, 28); string = (fun () -> "na_____me3");
          } );
        ( Eol, 
          { starts = (1, 28); ends = (2, 1);  string = (fun () -> "\\n") } );
        ( White_space,
          { starts = (2, 1);  ends = (2, 6);  string = (fun () -> " ") } );
        ( Name "....na..me.4",
          {
            starts = (2, 6);  ends = (2, 18); string = (fun () -> "....na..me.4");
          } );
        ( Eof, 
          { starts = (2, 18); ends = (2, 19); string = (fun () -> "EOF") } );
      ];
  ][@@ocamlformat "disable"]

let suite =
  List.concat
    [
      List.map ~f:Single_token_test.to_test_case test_single_token;
      List.map ~f:Multiple_token_test.to_test_case test_multiple_tokens;
    ]
