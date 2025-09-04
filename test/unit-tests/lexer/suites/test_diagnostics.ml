open! Import

let run name content () =
  Stdlib.Printf.printf "----- %s -----\n\n" name;
  Lexer_for_tests.with_lexer content @@ fun lexer ->
  let _ = Lexer.to_list lexer in
  Stdlib.Printf.printf "\n--------------\n\n"

let suite =
  [
    run "Incorrect number literal" "0x123421lol\n0z2141235 42r\n0xff 0x22 0x33";
    run "Incorrect escaped sequence" "\"sdsdga\\uqwqw\"";
  ]
