open! Import

module Private = struct
  open Mock_isa

  let create_lexer inp_m inp =
    let open Diagnostics_handler in
    Ocasm_lexer.create
      (module Mock_token)
      inp_m inp
      (module Kitchen_sink_handler)
      (Kitchen_sink_handler.create ())

  module Single_token_test : sig
    type t

    val mk : string -> string -> Mock_token.t Token.t -> t
    val to_test_case : t -> unit Alcotest.test_case
  end = struct
    type t = string * string * Mock_token.t Token.t

    let mk name content expected = (name, content, expected)

    let run content expected () =
      let module I = Ocasm_lexer.Input.StringInput in
      let input = I.create content in
      Input.with_input
        (module I)
        input
        ~f:(fun inp ->
          let lexer = create_lexer (module I) inp in
          let got = Ocasm_lexer.next_token lexer |> Option.value_exn |> fst in
          Alcotest.check
            (Testable.token Testable.mock_token)
            "Tokens don't coincide" expected got)

    let to_test_case (name, content, expected) =
      Alcotest.test_case name `Quick @@ run content expected
  end

  module Multiple_token_test : sig
    type t

    val mk : string -> string -> (Mock_token.t Token.t * Token_info.t) list -> t
    val to_test_case : t -> unit Alcotest.test_case
  end = struct
    type t = string * string * (Mock_token.t Token.t * Token_info.t) list

    let mk name content expected = (name, content, expected)

    let run content expected () =
      let module I = Ocasm_lexer.Input.StringInput in
      let input = I.create content in
      Input.with_input
        (module I)
        input
        ~f:(fun inp ->
          let lexer = create_lexer (module I) inp in
          Ocasm_lexer.to_list lexer |> Option.value_exn |> List.zip_exn expected
          |> List.iter ~f:(fun (expected, got) ->
                 Alcotest.check
                   (Alcotest.pair
                      (Testable.token Testable.mock_token)
                      Testable.token_info)
                   "Tokens don't coincide" expected got))

    let to_test_case (name, content, expected) =
      Alcotest.test_case name `Quick @@ run content expected
  end

  module Diagnostics_test : sig
    type t

    val mk : string -> string -> Diagnostic_message.Dyn.t list -> t
    val to_test_case : t -> unit Alcotest.test_case
  end = struct
    type t = string * string * Diagnostic_message.Dyn.t list

    let mk name content expected = (name, content, expected)

    let run content expected () =
      let module I = Ocasm_lexer.Input.StringInput in
      let input = I.create content in
      Input.with_input
        (module I)
        input
        ~f:(fun inp ->
          let open Diagnostics_handler in
          let handler = Kitchen_sink_handler.create () in
          let lexer =
            Ocasm_lexer.create
              (module Mock_token)
              (module I)
              inp
              (module Kitchen_sink_handler)
              handler
          in
          let _ = Ocasm_lexer.to_list lexer in
          List.iter
            (Kitchen_sink_handler.to_list handler |> List.zip_exn expected)
            ~f:(fun (expected, got) ->
              Alcotest.check Testable.diagnostic_message "Tokens don't coincide"
                expected got))

    let to_test_case (name, content, expected) =
      Alcotest.test_case name `Quick @@ run content expected
  end
end

open Private

let test_single_token =
  let open Token in
  let open Single_token_test in
  [
    mk "Binary"                       "0b1010"          (Bin (Big_integer.of_int64 0b1010L ));
    mk "Octal"                        "0000242455"      (Oct (Big_integer.of_int64 0o000242455L ));
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
    (* mk  *)
    (*   "Binary long wonky length (255)" *)
    (*   "0b101011000100110100101110110100110100101101010011010010110101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010" *)
    (*   (Bin *)
    (*      [| *)
    (*        0b0110100101101001101001011010010110100110100101101001101001011010L; *)
    (*        0b0110100110100101101001011010011010010110100110100101101001011010L; *)
    (*        0b1010010110100101101001101001011010011010010110100101101001101001L; *)
    (*        0b0101011000100110100101110110100110100101101010011010010110101001L; *)
    (*      |]); *)
    (* mk  *)
    (*   "Binary long normal length (256)" *)
    (*   "0b1101011000100110100101110110100110100101101010011010010110101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010010110100110100101101001101001011010" *)
    (*   (Bin *)
    (*      [| *)
    (*        0b0110100101101001101001011010010110100110100101101001101001011010L; *)
    (*        0b0110100110100101101001011010011010010110100110100101101001011010L; *)
    (*        0b1010010110100101101001101001011010011010010110100101101001101001L; *)
    (*        0b1101011000100110100101110110100110100101101010011010010110101001L; *)
    (*      |]); *)
  ][@@ocamlformat "disable"]

let test_multiple_tokens =
  let open Token in
  let open Multiple_token_test in
  [
    mk 
      "Curly brackets" 
      "{}"
      [
        (LCurly, { starts = Location.create 1 1; ends = Location.create 1 2; string = (fun () -> "{") });
        (RCurly, { starts = Location.create 1 2; ends = Location.create 1 3; string = (fun () -> "}") });
        (Eof,    { starts = Location.create 1 3; ends = Location.create 1 4; string = (fun () -> "EOF") });
      ];
    mk 
      "Three \\n's" 
      "\n \n\n"
      [
        ( Eol, 
          { starts = Location.create 1 1; ends = Location.create 2 1; string = (fun () -> "\\n") } );
        ( White_space,
          { starts = Location.create 2 1; ends = Location.create 2 2; string = (fun () -> " ") } );
        ( Eol, 
          { starts = Location.create 2 2; ends = Location.create 3 1; string = (fun () -> "\\n") } );
        ( Eol, 
          { starts = Location.create 3 1; ends = Location.create 4 1; string = (fun () -> "\\n") } );
        ( Eof, 
          { starts = Location.create 4 1; ends = Location.create 4 2; string = (fun () -> "EOF") } );
      ];
    mk 
      "Brackets and single line comment" 
      "() # a single line comment\n()"
      [
        ( LBracket, 
          { starts = Location.create 1 1;  ends = Location.create 1 2;  string = (fun () -> "(") } );
        ( RBracket, 
          { starts = Location.create 1 2;  ends = Location.create 1 3;  string = (fun () -> ")") } );
        ( White_space,
          { starts = Location.create 1 3;  ends = Location.create 1 27; string = (fun () -> " ") } );
        ( Eol, 
          { starts = Location.create 1 27; ends = Location.create 2 1;  string = (fun () -> "\\n") } );
        ( LBracket, 
          { starts = Location.create 2 1;  ends = Location.create 2 2;  string = (fun () -> "(") } );
        ( RBracket, 
          { starts = Location.create 2 2;  ends = Location.create 2 3;  string = (fun () -> ")") } );
        ( Eof, 
          { starts = Location.create 2 3;  ends = Location.create 2 4;  string = (fun () -> "EOF") } );
      ];
    mk "4 Names" "name1    name2\t \tna_____me3\n    \t....na..me.4"
      [
        ( Name "name1",
          { starts = Location.create 1 1;  ends = Location.create 1 6;  string = (fun () -> "name1") } );
        ( White_space,
          { starts = Location.create 1 6;  ends = Location.create 1 10; string = (fun () -> " ") } );
        ( Name "name2",
          { starts = Location.create 1 10; ends = Location.create 1 15; string = (fun () -> "name2") } );
        ( White_space,
          { starts = Location.create 1 15; ends = Location.create 1 18; string = (fun () -> " ") } );
        ( Name "na_____me3",
          {
            starts = Location.create 1 18; ends = Location.create 1 28; string = (fun () -> "na_____me3");
          } );
        ( Eol, 
          { starts = Location.create 1 28; ends = Location.create 2 1;  string = (fun () -> "\\n") } );
        ( White_space,
          { starts = Location.create 2 1;  ends = Location.create 2 6;  string = (fun () -> " ") } );
        ( Name "....na..me.4",
          {
            starts = Location.create 2 6;  ends = Location.create 2 18; string = (fun () -> "....na..me.4");
          } );
        ( Eof, 
          { starts = Location.create 2 18; ends = Location.create 2 19; string = (fun () -> "EOF") } );
      ];
  ][@@ocamlformat "disable"]

let test_error_messages =
  let open Diagnostics_test in
  let open Diagnostic_message in
  [
    mk "Incorrect number literal" "0x123421lol\n0z2141235 42r\n0xff 0x22 0x33"
      [
        Dyn
          {
            typ = Error;
            msg = "unexpected character 'l' in a number literal";
            id = 4;
            starts = Location.create 1 9;
            ends = Location.create 1 9;
            file = Path.of_string "";
            ctx = "";
          };
        Dyn
          {
            typ = Error;
            msg = "unexpected character 'z' in a number literal";
            id = 4;
            starts = Location.create 2 2;
            ends = Location.create 2 2;
            file = Path.of_string "";
            ctx = "";
          };
      ];
    mk "Incorrect escaped sequence" "\"sdsdga\\uqwqw\""
      [
        Dyn
          {
            typ = Error;
            msg = "incorrect escape sequence '\\u'";
            id = 0;
            starts = Location.create 1 10;
            ends = Location.create 1 10;
            file = Path.of_string "";
            ctx = "";
          };
      ];
  ]

let suite =
  List.concat
    [
      List.map ~f:Single_token_test.to_test_case test_single_token;
      List.map ~f:Multiple_token_test.to_test_case test_multiple_tokens;
      List.map ~f:Diagnostics_test.to_test_case test_error_messages;
    ]
