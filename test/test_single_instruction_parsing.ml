open Alcotest
open Rv32.Parser_error
open Rv32.Parser_frontend
open Rv32.Instruction
open Rv32.Register
open Rv32.Immediate

let structured_instruction_fmt fmt instr =
  Format.fprintf fmt "%s" (show_structured_instruction instr)

let structured_instruction =
  Alcotest.testable structured_instruction_fmt equal_structured_instruction

let parser_error =
  Alcotest.testable
    (fun fmt err -> Format.fprintf fmt "%s" (show_parser_error err))
    equal_parser_error

let parser_result = Alcotest.result (list structured_instruction) parser_error

let test_empty_program () =
  let result = parse_program "" in
  check parser_result "empty program" (Ok []) result

let test_add_imm () =
  let program = "add x2, x1, 5" in
  let expected =
    [ Add (x2, x1, Imm (Immediate12.of_int32 5l |> Option.get)) ]
  in
  let result = parse_program program in
  check parser_result "add immediate" (Ok expected) result

let test_add_reg () =
  let program = "add x2, x1, x3" in
  let expected = [ Add (x2, x1, Reg x3) ] in
  let result = parse_program program in
  check parser_result "add register" (Ok expected) result

let test_wrong_lexeme () =
  let program = "add y3, x1, x2" in
  let result = parse_program program in
  check parser_result "Wrong lexeme error message"
    (Error (ParsingError (InvalidRegister "y3"))) result

let test_invalid_syntax () =
  let program = "add x1, x2" in
  let result = parse_program program in
  check parser_result "Invalid syntax error message"
    (Error
       (ParsingError
          (InvalidSyntax
             ( { pos_fname = ""; pos_lnum = 1; pos_cnum = 10; pos_bol = 0 },
               { pos_fname = ""; pos_lnum = 1; pos_cnum = 10; pos_bol = 0 } ))))
    result

let test_subtract () =
  let program = "sub x1, x2, x3" in
  let expected = [ Sub (x1, x2, x3) ] in
  let result = parse_program program in
  check parser_result "Subtract" (Ok expected) result

let test_and () =
  let program = "and x1, x2, x3" in
  let expected = [ And (x1, x2, Reg x3) ] in
  let result = parse_program program in
  check parser_result "And" (Ok expected) result

let test_or () =
  let program = "or x1, x2, x3" in
  let expected = [ Or (x1, x2, Reg x3) ] in
  let result = parse_program program in
  check parser_result "Or" (Ok expected) result

let test_xor () =
  let program = "xor x1, x2, x3" in
  let expected = [ Xor (x1, x2, Reg x3) ] in
  let result = parse_program program in
  check parser_result "Xor" (Ok expected) result

let test_slt () =
  let program = "slt x1, x2, x3" in
  let expected = [ Slt (x1, x2, Reg x3) ] in
  let result = parse_program program in
  check parser_result "Slt" (Ok expected) result

let test_sltu () =
  let program = "sltu x1, x2, x3" in
  let expected = [ Sltu (x1, x2, Reg x3) ] in
  let result = parse_program program in
  check parser_result "Sltu" (Ok expected) result

let test_andi () =
  let program = "and x3, x2, 42" in
  let expected =
    [ And (x3, x2, Imm (Immediate12.of_int32 42l |> Option.get)) ]
  in
  let result = parse_program program in
  check parser_result "And immediate" (Ok expected) result

let test_ori () =
  let program = "or x3, x2, 42" in
  let expected =
    [ Or (x3, x2, Imm (Immediate12.of_int32 42l |> Option.get)) ]
  in
  let result = parse_program program in
  check parser_result "Or immediate" (Ok expected) result

let test_xori () =
  let program = "xor x3, x2, 42" in
  let expected =
    [ Xor (x3, x2, Imm (Immediate12.of_int32 42l |> Option.get)) ]
  in
  let result = parse_program program in
  check parser_result "Xor immediate" (Ok expected) result

let test_slti () =
  let program = "slt x3, x2, 42" in
  let expected =
    [ Slt (x3, x2, Imm (Immediate12.of_int32 42l |> Option.get)) ]
  in
  let result = parse_program program in
  check parser_result "Slt immediate" (Ok expected) result

let test_sltui () =
  let program = "sltu x3, x2, 42" in
  let expected =
    [ Sltu (x3, x2, Imm (Immediate12.of_int32 42l |> Option.get)) ]
  in
  let result = parse_program program in
  check parser_result "Sltu immediate" (Ok expected) result

let test_invalid_immediate () =
  let program = "add x3, x2, 214748364" in
  let result = parse_program program in
  check parser_result "Invalid immediate"
    (Error (ParsingError (InvalidImmediate 214748364l))) result

let test_beq () =
  let program = "beq x2, x1, 42" in
  let expected = [ Beq (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Beq" (Ok expected) result

let test_bne () =
  let program = "bne x2, x1, 42" in
  let expected = [ Bne (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Bne" (Ok expected) result

let test_blt () =
  let program = "blt x2, x1, 42" in
  let expected = [ Blt (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Blt" (Ok expected) result

let test_bge () =
  let program = "bge x2, x1, 42" in
  let expected = [ Bge (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Bge" (Ok expected) result

let test_bltu () =
  let program = "bltu x2, x1, 42" in
  let expected = [ Bltu (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Bltu" (Ok expected) result

let test_bgeu () =
  let program = "bgeu x2, x1, 42" in
  let expected = [ Bgeu (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Bgeu" (Ok expected) result

let test_lb () =
  let program = "lb x2, x1, 42" in
  let expected = [ Lb (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Lb" (Ok expected) result

let test_lh () =
  let program = "lh x2, x1, 42" in
  let expected = [ Lh (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Lh" (Ok expected) result

let test_lw () =
  let program = "lw x2, x1, 42" in
  let expected = [ Lw (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Lw" (Ok expected) result

let test_lbu () =
  let program = "lbu x2, x1, 42" in
  let expected = [ Lbu (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Lbu" (Ok expected) result

let test_lhu () =
  let program = "lhu x2, x1, 42" in
  let expected = [ Lhu (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Lhu" (Ok expected) result

let test_sb () =
  let program = "sb x2, x1, 42" in
  let expected = [ Sb (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Sb" (Ok expected) result

let test_sh () =
  let program = "sh x2, x1, 42" in
  let expected = [ Sh (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Sh" (Ok expected) result

let test_sw () =
  let program = "sw x2, x1, 42" in
  let expected = [ Sw (x2, x1, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Sw" (Ok expected) result

let test_jal () =
  let program = "jal x1, 42" in
  let expected = [ Jal (x1, Immediate20.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Jal" (Ok expected) result

let test_jalr () =
  let program = "jalr x1, x2, 42" in
  let expected = [ Jalr (x1, x2, Immediate12.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Jalr" (Ok expected) result

let test_lui () =
  let program = "lui x1, 42" in
  let expected = [ Lui (x1, Immediate20.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Lui" (Ok expected) result

let test_auipc () =
  let program = "auipc x1, 42" in
  let expected = [ Auipc (x1, Immediate20.of_int32 42l |> Option.get) ] in
  let result = parse_program program in
  check parser_result "Auipc" (Ok expected) result

let test_ecall () =
  let program = "ecall" in
  let expected = [ Ecall ] in
  let result = parse_program program in
  check parser_result "Ecall" (Ok expected) result

let test_ebreak () =
  let program = "ebreak" in
  let expected = [ Ebreak ] in
  let result = parse_program program in
  check parser_result "Ebreak" (Ok expected) result

let suite =
  [
    test_case "Empty program" `Quick test_empty_program;
    test_case "Add immediate" `Quick test_add_imm;
    test_case "Add register" `Quick test_add_reg;
    test_case "Wrong lexeme" `Quick test_wrong_lexeme;
    test_case "Invalid syntax" `Quick test_invalid_syntax;
    test_case "Subtract" `Quick test_subtract;
    test_case "And" `Quick test_and;
    test_case "Or" `Quick test_or;
    test_case "Xor" `Quick test_xor;
    test_case "Slt" `Quick test_slt;
    test_case "Sltu" `Quick test_sltu;
    test_case "And immediate" `Quick test_andi;
    test_case "Or immediate" `Quick test_ori;
    test_case "Xor immediate" `Quick test_xori;
    test_case "Slt immediate" `Quick test_slti;
    test_case "Sltu immediate" `Quick test_sltui;
    test_case "Invalid immediate" `Quick test_invalid_immediate;
    test_case "Beq" `Quick test_beq;
    test_case "Bne" `Quick test_bne;
    test_case "Blt" `Quick test_blt;
    test_case "Bge" `Quick test_bge;
    test_case "Bltu" `Quick test_bltu;
    test_case "Bgeu" `Quick test_bgeu;
    test_case "Lb" `Quick test_lb;
    test_case "Lh" `Quick test_lh;
    test_case "Lw" `Quick test_lw;
    test_case "Lbu" `Quick test_lbu;
    test_case "Lhu" `Quick test_lhu;
    test_case "Sb" `Quick test_sb;
    test_case "Sh" `Quick test_sh;
    test_case "Sw" `Quick test_sw;
    test_case "Jal" `Quick test_jal;
    test_case "Jalr" `Quick test_jalr;
    test_case "Lui" `Quick test_lui;
    test_case "Auipc" `Quick test_auipc;
    test_case "Ecall" `Quick test_ecall;
    test_case "Ebreak" `Quick test_ebreak;
  ]
