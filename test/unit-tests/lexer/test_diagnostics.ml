open! Import

let mk name content expected =
  let open Diagnostics_handler in
  Alcotest.test_case name `Quick @@ fun () ->
  Lexer_for_tests.with_lexer_and_handler content @@ fun lexer handler ->
  let _ = Lexer.to_list lexer in
  List.iter
    (Kitchen_sink_handler.to_list handler |> List.zip_exn expected)
    ~f:(fun (expected, got) ->
      Alcotest.check Testable.diagnostic_message "Tokens don't coincide"
        expected got)

let suite =
  let open Diagnostic_message in
  [
    mk "Incorrect number literal" "0x123421lol\n0z2141235 42r\n0xff 0x22 0x33"
      [
        Dyn
          {
            typ = Error;
            msg = "unexpected character 'l' in a number literal";
            starts = Location.create 1 9;
            ends = Location.create 1 9;
            file = Path.of_string "";
            ctx = "";
          };
        Dyn
          {
            typ = Error;
            msg = "unexpected character 'z' in a number literal";
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
            starts = Location.create 1 10;
            ends = Location.create 1 10;
            file = Path.of_string "";
            ctx = "";
          };
      ];
  ]
