open! Import

let mk name content expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Lexer_for_tests.with_lexer content @@ fun lexer ->
  Lexer.to_list lexer |> List.zip_exn expected
  |> List.iter ~f:(fun (expected, got) ->
         Alcotest.check
           (Alcotest.pair
              (Testable.token Testable.mock_token)
              Testable.token_info)
           "Tokens don't coincide" expected got)

let suite =
  let open Token in
  [
    mk 
      "Curly brackets" 
      "{}"
      [
        (LCurly, { starts = Location.create 1 1; ends = Location.create 1 2; string = "{" });
        (RCurly, { starts = Location.create 1 2; ends = Location.create 1 3; string = "}" });
        (Eof,    { starts = Location.create 1 3; ends = Location.create 1 4; string = "EOF" });
      ];
    mk 
      "Three \\n's" 
      "\n \n\n"
      [
        ( Eol, 
          { starts = Location.create 1 1; ends = Location.create 2 1; string = "\\n" } );
        ( White_space,
          { starts = Location.create 2 1; ends = Location.create 2 2; string = " " } );
        ( Eol, 
          { starts = Location.create 2 2; ends = Location.create 3 1; string = "\\n" } );
        ( Eol, 
          { starts = Location.create 3 1; ends = Location.create 4 1; string = "\\n" } );
        ( Eof, 
          { starts = Location.create 4 1; ends = Location.create 4 2; string = "EOF" } );
      ];
    mk 
      "Brackets and single line comment" 
      "() # a single line comment\n()"
      [
        ( LBracket, 
          { starts = Location.create 1 1;  ends = Location.create 1 2;  string = "(" } );
        ( RBracket, 
          { starts = Location.create 1 2;  ends = Location.create 1 3;  string = ")" } );
        ( White_space,
          { starts = Location.create 1 3;  ends = Location.create 1 27; string = " " } );
        ( Eol, 
          { starts = Location.create 1 27; ends = Location.create 2 1;  string = "\\n" } );
        ( LBracket, 
          { starts = Location.create 2 1;  ends = Location.create 2 2;  string = "(" } );
        ( RBracket, 
          { starts = Location.create 2 2;  ends = Location.create 2 3;  string = ")" } );
        ( Eof, 
          { starts = Location.create 2 3;  ends = Location.create 2 4;  string = "EOF" } );
      ];
    mk "4 Names" "name1    name2\t \tna_____me3\n    \t....na..me.4"
      [
        ( Name "name1",
          { starts = Location.create 1 1;  ends = Location.create 1 6;  string = "name1" } );
        ( White_space,
          { starts = Location.create 1 6;  ends = Location.create 1 10; string = " " } );
        ( Name "name2",
          { starts = Location.create 1 10; ends = Location.create 1 15; string = "name2" } );
        ( White_space,
          { starts = Location.create 1 15; ends = Location.create 1 18; string = " " } );
        ( Name "na_____me3",
          {
            starts = Location.create 1 18; ends = Location.create 1 28; string = "na_____me3";
          } );
        ( Eol, 
          { starts = Location.create 1 28; ends = Location.create 2 1;  string = "\\n" } );
        ( White_space,
          { starts = Location.create 2 1;  ends = Location.create 2 6;  string = " " } );
        ( Name "....na..me.4",
          {
            starts = Location.create 2 6;  ends = Location.create 2 18; string = "....na..me.4";
          } );
        ( Eof, 
          { starts = Location.create 2 18; ends = Location.create 2 19; string = "EOF" } );
      ];
  ][@@ocamlformat "disable"]
