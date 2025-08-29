open! Import

let mk name content expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Lexer_for_tests.with_lexer content @@ fun lexer ->
  Lexer.to_list lexer |> Option.value_exn |> List.zip_exn expected
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
