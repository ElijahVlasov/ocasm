open Alcotest
open Base
open Ocasm_parser

let test_string_input_first () =
  let module I = Input.StringInput in
  let content = "1 2 3 4 5 6 7 8 9\n 1 2 3 4 5 6 7 8 9" in
  let input = I.create ~content in
  let expected = '1' in
  let result = I.next input in
  Alcotest.check char "First char" expected result

let test_string_input_endline () =
  let module I = Input.StringInput in
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
    let module I = Input.FileInput in
    let rec read input buf =
      let ( = ) = Char.equal in
      let next_ch = I.next input in
      if next_ch = Input.eof then ()
      else (
        Buffer.add_char buf next_ch;
        read input buf)
    in
    Input.with_input
      (module I)
      (I.create ~path)
      ~f:(fun input ->
        let buf = Buffer.create (String.length expected) in
        read input buf;
        Buffer.contents buf)
  in
  Alcotest.check string "expected to be same strings" expected result

let test_cursor_back_forth (type a) csr_m (csr : a) ?buf_len expected =
  let module C = (val csr_m : Input.C with type t = a) in
  let open C in
  let rec roll_forward buf =
    Buffer.add_char buf (C.get csr);
    if step csr then roll_forward buf
  in
  let rec roll_backward buf =
    if back csr then (
      Buffer.add_char buf (C.get csr);
      roll_backward buf)
  in
  let got =
    let buf = Buffer.create (Option.value ~default:16 buf_len) in
    roll_forward buf;
    roll_backward buf;
    Buffer.contents buf
  in
  Alcotest.check string "Two string should be equal" expected got

let test_string_input_cursor_back_forth () =
  let content = "abcde" in
  let module I = Input.StringInput in
  let input = I.create ~content in
  let csr = I.start input in
  let expected = "abcdedcba" in
  test_cursor_back_forth (module I.Cursor) csr expected

let test_string_input_cursor_back_forth_two_chars_forward () =
  let content = "abcde" in
  let module I = Input.StringInput in
  let input =
    let input = I.create ~content in
    I.skip input;
    I.skip input;
    input
  in
  let csr = I.start input in
  let expected = "cdedc" in
  test_cursor_back_forth (module I.Cursor) csr expected

let test_file_input_cursor_back_forth expected path =
  let module I = Input.FileInput in
  Input.with_input
    (module I)
    (I.create ~path)
    ~f:(fun input ->
      let csr = I.start input in
      test_cursor_back_forth
        ~buf_len:(2 * String.length expected)
        (module I.Cursor)
        csr
        (String.concat [ expected; String.subo ~pos:1 @@ String.rev expected ]))

let test_short_file_input_cursor_back_forth path () =
  let expected =
    let module I = Stdlib.In_channel in
    I.with_open_text path I.input_all
  in
  test_file_input_cursor_back_forth expected path

let test_long_file_input_cursor_back_forth path () =
  let expected =
    String.subo ~len:(2 * 4096)
    @@
    let module I = Stdlib.In_channel in
    I.with_open_text path I.input_all
  in
  test_file_input_cursor_back_forth expected path

let test_string_input =
  [
    test_case "StringInput.next 1" `Quick test_string_input_first;
    test_case "StringInput.next 2" `Quick test_string_input_endline;
  ]

let test_file_input =
  [
    test_case "Short File" `Quick
    @@ test_file_input_entire_file "assets/input/short_file.txt";
    test_case "Long file 1" `Quick
    @@ test_file_input_entire_file "assets/input/long_file_1.txt";
    test_case "Long file 2" `Quick
    @@ test_file_input_entire_file "assets/input/long_file_2.txt";
    test_case "Long file 3" `Quick
    @@ test_file_input_entire_file "assets/input/long_file_3.txt";
  ]

let test_string_input_cursor =
  [
    test_case "Cursor back and forth" `Quick test_string_input_cursor_back_forth;
    test_case "Cursor back and forth starting two chars into the string" `Quick
      test_string_input_cursor_back_forth_two_chars_forward;
  ]

let test_file_input_cursor =
  [
    test_case "Cursor back and forth for a short file" `Quick
    @@ test_short_file_input_cursor_back_forth "assets/input/short_file.txt";
    test_case "Cursor back and forth for a long file" `Quick
    @@ test_long_file_input_cursor_back_forth "assets/input/long_file_1.txt";
  ]

let () =
  Alcotest.run "Parser input"
    [
      ("StringInput", test_string_input);
      ("FileInput", test_file_input);
      ("Cursor StringInput", test_string_input_cursor);
      ("Cursor FileInput", test_file_input_cursor);
    ]
