open Alcotest
open Base
open Ocasm_lexer

let short_file = "assets/input/short_file.txt"
let long_file_1 = "assets/input/long_file_1.txt"
let long_file_2 = "assets/input/long_file_2.txt"
let long_file_3 = "assets/input/long_file_3.txt"
let multiline = "assets/input/multilines.txt"

let test_string_input_first () =
  let module I = Input.StringInput in
  let content = "1 2 3 4 5 6 7 8 9\n 1 2 3 4 5 6 7 8 9" in
  let input = I.create ~content in
  let expected = '1' in
  let result = I.next input in
  Alcotest.check char "The first read char is wrong" expected result

let test_string_input_endline () =
  let module I = Input.StringInput in
  let module F = Base.Fn in
  let content = "1 2 3 4 5 6 7 8 9\n 1 2 3 4 5 6 7 8 9" in
  let input = I.create ~content in
  let expected = '\n' in
  let result = I.next_n_times ~n:18 input in
  Alcotest.check char "Expected '\\n'" expected result

let test_file_input_entire_file path () =
  let expected =
    let module I = Stdlib.In_channel in
    I.with_open_text path @@ fun in_ch -> I.input_all in_ch
  in
  let result =
    let module I = Input.FileInput in
    Input.with_input
      (module I)
      (I.create ~path)
      ~f:(fun input ->
        let buf = Buffer.create (String.length expected) in
        I.read_buf input buf;
        Buffer.contents buf)
  in
  Alcotest.check string "Expected to be same strings" expected result

let test_string_input =
  [
    test_case "StringInput.next 1" `Quick test_string_input_first;
    test_case "StringInput.next 2" `Quick test_string_input_endline;
  ]

let test_file_input =
  [
    test_case "Short File" `Quick @@ test_file_input_entire_file short_file;
    test_case "Long file 1" `Quick @@ test_file_input_entire_file long_file_1;
    test_case "Long file 2" `Quick @@ test_file_input_entire_file long_file_2;
    test_case "Long file 3" `Quick @@ test_file_input_entire_file long_file_3;
  ]

let suite = List.concat [ test_string_input; test_file_input ]
