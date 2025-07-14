open Alcotest
open Rv32.Parser_error
open Rv32.Parser_frontend
open Rv32.Instruction
open Rv32.Register

let structured_instruction_fmt fmt instr =
  Format.fprintf fmt "%s" (show_structured_instruction instr)

let structured_instruction =
  Alcotest.testable structured_instruction_fmt equal_structured_instruction

let parser_error =
  Alcotest.testable
    (fun fmt err -> Format.fprintf fmt "%s" (show_parser_error err))
    equal_parser_error

let parser_result = Alcotest.result (list structured_instruction) parser_error

let test_add_add_add () =
  let program =
    "\n\
    \  add x1, x2, x3\n\
    \  add x4, x5, x6 # this is a comment    \n\
    \  add x7, \n\
    \           x8\n\
    \           /* this is a multi-line comment \n\
    \           with a newline */\n\
    \           \n\
    \           , \n\
    \           \n\
    \           \n\
    \           x9"
  in
  let expected =
    [ Add (x1, x2, Reg x3); Add (x4, x5, Reg x6); Add (x7, x8, Reg x9) ]
  in
  let result = parse_program program in
  Alcotest.check parser_result "add add add" (Ok expected) result

let suite = [ test_case "add add add" `Quick test_add_add_add ]
