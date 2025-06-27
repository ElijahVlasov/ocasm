open Alcotest

let test_string_input_first () =
  let module I = Ocasm_parser.Input.StringInput in
  let content = "1 2 3 4 5 6 7 8 9\n 1 2 3 4 5 6 7 8 9" in
  let input = I.create ~content in
  let expected = '1' in
  let result = I.next input in
  Alcotest.check char "First char" expected result

let test_string_input_endline () =
  let module I = Ocasm_parser.Input.StringInput in
  let module F = Base.Fn in
  let content = "1 2 3 4 5 6 7 8 9\n 1 2 3 4 5 6 7 8 9" in
  let input = I.create ~content in
  F.apply_n_times ~n:17
    (fun () ->
      let _ = I.next input in
      ())
    ();
  let expected = '\n' in
  let result = I.next input in
  Alcotest.check char "Expected endline" expected result

let test_file_input_entire_file path () =
  let expected =
    let module I = Stdlib.In_channel in
    I.with_open_text path @@ fun in_ch -> I.input_all in_ch
  in
  let result =
    let module I = Ocasm_parser.Input.FileInput in
    let rec read input buf =
      let ( = ) = Char.equal in
      let next_ch = I.next input in
      if next_ch = Ocasm_parser.Input.eof then ()
      else (
        Buffer.add_char buf next_ch;
        read input buf)
    in
    Ocasm_parser.Input.with_input
      (module I)
      (I.create ~path)
      ~f:(fun input ->
        let buf = Buffer.create (String.length expected) in
        read input buf;
        Buffer.contents buf)
  in
  Alcotest.check string "expected to be same strings" expected result

let () =
  Alcotest.run "Parser input"
    [
      ( "StringInput",
        [
          test_case "StringInput.next 1" `Quick test_string_input_first;
          test_case "StringInput.next 2" `Quick test_string_input_endline;
        ] );
      ( "FileInput",
        [
          test_case "Short File" `Quick
          @@ test_file_input_entire_file "assets/input/short_file.txt";
          test_case "Long file 1" `Quick
          @@ test_file_input_entire_file "assets/input/long_file_1.txt";
          test_case "Long file 2" `Quick
          @@ test_file_input_entire_file "assets/input/long_file_2.txt";
          test_case "Long file 3" `Quick
          @@ test_file_input_entire_file "assets/input/long_file_3.txt";
        ] );
    ]
