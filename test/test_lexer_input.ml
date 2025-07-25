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

let test_cursor_back_forth (type a) csr_m (csr : a) ?buf_len expected =
  let module C = (val csr_m : Input.C0 with type t = a) in
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
  let csr = I.Cursor.create input in
  let expected = "abcdedcba" in
  test_cursor_back_forth (module I.Cursor) csr expected

let test_string_input_cursor_back_forth_two_chars_forward () =
  let content = "abcde" in
  let module I = Input.StringInput in
  let input =
    let input = I.create ~content in
    I.skip_n_times ~n:2 input;
    input
  in
  let csr = I.Cursor.create input in
  let expected = "cdedc" in
  test_cursor_back_forth (module I.Cursor) csr expected

let test_file_input_cursor_back_forth expected path =
  let module I = Input.FileInput in
  Input.with_input
    (module I)
    (I.create ~path)
    ~f:(fun input ->
      let csr = I.Cursor.create input in
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

let test_line_col_numbers () =
  let path = multiline in
  let module I = Input.FileInput in
  let input = I.create ~path in
  let module I = Input.MakePositioned (Input.FileInput) in
  let input = I.create input in
  Input.with_input
    (module I)
    input
    ~f:(fun input ->
      let csr = I.Cursor.create input in
      let move n =
        Fn.apply_n_times ~n (fun () -> Fn.ignore @@ I.Cursor.step csr) ()
      in
      let positions = [] in
      move 15;
      I.advance input csr;
      let pos = I.pos input in
      let positions = List.cons pos positions in
      move 26;
      I.advance input csr;
      let pos = I.pos input in
      let positions = List.cons pos positions in
      Alcotest.check
        (list (pair int int))
        "Cursor checkpoints do not match"
        [ (3, 3); (2, 2) ]
        positions)

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

let test_string_input_cursor =
  [
    test_case "Cursor back and forth" `Quick test_string_input_cursor_back_forth;
    test_case "Cursor back and forth 2 chars later" `Quick
      test_string_input_cursor_back_forth_two_chars_forward;
  ]

let test_file_input_cursor =
  [
    test_case "Cursor back and forth for a short file" `Quick
    @@ test_short_file_input_cursor_back_forth short_file;
    test_case "Cursor back and forth for a long file" `Quick
    @@ test_long_file_input_cursor_back_forth long_file_1;
  ]

let test_positioned_input =
  [ test_case "Test correct positioning" `Quick test_line_col_numbers ]

let suite =
  List.concat
    [
      test_string_input;
      test_file_input;
      test_string_input_cursor;
      test_file_input_cursor;
      test_positioned_input;
    ]
