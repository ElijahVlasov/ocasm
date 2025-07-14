open Rv32.Immediate

let imm12_fmt fmt immediate =
  Format.fprintf fmt "%s" (Immediate12.to_string immediate)

let imm12 = Alcotest.testable imm12_fmt equal_imm12

let test_immediate12_positive () =
  Alcotest.(check int32)
    "positive immediate12 value" 0x123l
    (Immediate12.to_int32 (Immediate12.of_int32_unchecked 0x123l))

let test_immediate12_negative () =
  Alcotest.(check int32)
    "negative immediate12 value" (-1l)
    (Immediate12.to_int32 (Immediate12.of_int32_unchecked 0xfffl))

let test_immediate12_of_int32_unchecked () =
  Alcotest.(check imm12)
    "immediate12 of int32_unchecked"
    (Option.get (Immediate12.of_int32 0x123l))
    (Immediate12.of_int32_unchecked 0x123l)

let test_immediate12_back_and_forth () =
  Alcotest.(check int32)
    "immediate12 back and forth" 0x123l
    (Immediate12.to_int32 (Option.get (Immediate12.of_int32 0x123l)))

let test_immediate12_back_and_forth_negative () =
  Alcotest.(check int32)
    "immediate12 back and forth negative" (-56l)
    (Immediate12.to_int32 (Option.get (Immediate12.of_int32 (-56l))))

let suite =
  let open Alcotest in
  [
    test_case "positive" `Quick test_immediate12_positive;
    test_case "negative" `Quick test_immediate12_negative;
    test_case "back and forth" `Quick test_immediate12_back_and_forth;
    test_case "back and forth negative" `Quick
      test_immediate12_back_and_forth_negative;
    test_case "of int32_unchecked" `Quick test_immediate12_of_int32_unchecked;
  ]
