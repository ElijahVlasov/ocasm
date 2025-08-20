open! Import
open Rv32.Instruction
open Rv32.Register
open Rv32.Fence_type
open Rv32.Immediate

let raw_instruction_fmt fmt raw_instruction =
  Stdlib.Format.fprintf fmt "%s" (RawInstruction.to_string raw_instruction)

let raw_instruction = Alcotest.testable raw_instruction_fmt RawInstruction.eq_t

let test_add_x0_x0_x0 () =
  Alcotest.(check raw_instruction)
    "add x0, x0, x0"
    (RawInstruction.of_int 0x00000033l)
    (RawInstruction.add x0 x0 (Reg x0))

let test_add_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "add x1, x2, x3"
    (RawInstruction.of_int 0x003100b3l)
    (RawInstruction.add x1 x2 (Reg x3))

let test_addi_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "addi x1, x2, 3"
    (RawInstruction.of_int 0x00310093l)
    (RawInstruction.add x1 x2
       (Imm (Option.value_exn (Immediate12.of_int32 3l))))

let test_addi_x0_x0_imm_neg () =
  Alcotest.(check raw_instruction)
    "addi x0, x0, -42"
    (RawInstruction.of_int 0xfd600013l)
    (RawInstruction.add x0 x0
       (Imm (Option.value_exn (Immediate12.of_int32 (Int32.neg 42l)))))

let test_sub_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "sub x1, x2, x3"
    (RawInstruction.of_int 0x403100b3l)
    (RawInstruction.sub x1 x2 x3)

let test_sll_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "sll x1, x2, x3"
    (RawInstruction.of_int 0x003110b3l)
    (RawInstruction.sll x1 x2 (Reg x3))

let test_srl_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "srl x1, x2, x3"
    (RawInstruction.of_int 0x003150b3l)
    (RawInstruction.srl x1 x2 (Reg x3))

let test_xor_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "xor x1, x2, x3"
    (RawInstruction.of_int 0x003140b3l)
    (RawInstruction.xor x1 x2 (Reg x3))

let test_or_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "or x1, x2, x3"
    (RawInstruction.of_int 0x003160b3l)
    (RawInstruction.or_ x1 x2 (Reg x3))

let test_and_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "and x1, x2, x3"
    (RawInstruction.of_int 0x003170b3l)
    (RawInstruction.and_ x1 x2 (Reg x3))

let test_slt_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "slt x1, x2, x3"
    (RawInstruction.of_int 0x003120b3l)
    (RawInstruction.slt x1 x2 (Reg x3))

let test_sltu_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "sltu x1, x2, x3"
    (RawInstruction.of_int 0x003130b3l)
    (RawInstruction.sltu x1 x2 (Reg x3))

let test_sra_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "sra x1, x2, x3"
    (RawInstruction.of_int 0x403150b3l)
    (RawInstruction.sra x1 x2 (Reg x3))

let test_lui_x1_1 () =
  Alcotest.(check raw_instruction)
    "lui x1, 1"
    (RawInstruction.of_int 0x000010b7l)
    (RawInstruction.lui x1 (Option.value_exn (Immediate20.of_int32 1l)))

let test_lui_x0_imm_neg () =
  Alcotest.(check raw_instruction)
    "lui x0, -42"
    (RawInstruction.of_int 0xfffd6037l)
    (RawInstruction.lui x0
       (Option.value_exn (Immediate20.of_int32 (Int32.neg 42l))))

let test_auipc_x1_1 () =
  Alcotest.(check raw_instruction)
    "auipc x1, 1"
    (RawInstruction.of_int 0x00001097l)
    (RawInstruction.auipc x1 (Option.value_exn (Immediate20.of_int32 1l)))

let test_auipc_x0_imm_neg () =
  Alcotest.(check raw_instruction)
    "auipc x0, -42"
    (RawInstruction.of_int 0xfffd6017l)
    (RawInstruction.auipc x0
       (Option.value_exn (Immediate20.of_int32 (Int32.neg 42l))))

let test_lb_x1_1_x2 () =
  Alcotest.(check raw_instruction)
    "lb x1, 1(x2)"
    (RawInstruction.of_int 0x00110083l)
    (RawInstruction.lb x1 (Option.value_exn (Immediate12.of_int32 1l)) x2)

let test_lh_x1_1_x2 () =
  Alcotest.(check raw_instruction)
    "lh x1, 1(x2)"
    (RawInstruction.of_int 0x00111083l)
    (RawInstruction.lh x1 (Option.value_exn (Immediate12.of_int32 1l)) x2)

let test_lw_x1_1_x2 () =
  Alcotest.(check raw_instruction)
    "lw x1, 1(x2)"
    (RawInstruction.of_int 0x00112083l)
    (RawInstruction.lw x1 (Option.value_exn (Immediate12.of_int32 1l)) x2)

let test_lbu_x1_1_x2 () =
  Alcotest.(check raw_instruction)
    "lbu x1, 1(x2)"
    (RawInstruction.of_int 0x00114083l)
    (RawInstruction.lbu x1 (Option.value_exn (Immediate12.of_int32 1l)) x2)

let test_lhu_x1_1_x2 () =
  Alcotest.(check raw_instruction)
    "lhu x1, 1(x2)"
    (RawInstruction.of_int 0x00115083l)
    (RawInstruction.lhu x1 (Option.value_exn (Immediate12.of_int32 1l)) x2)

let test_sb_x1_1_x2 () =
  Alcotest.(check raw_instruction)
    "sb x1, 1(x2)"
    (RawInstruction.of_int 0x001100a3l)
    (RawInstruction.sb x1 (Option.value_exn (Immediate12.of_int32 1l)) x2)

let test_sh_x1_1_x2 () =
  Alcotest.(check raw_instruction)
    "sh x1, 1(x2)"
    (RawInstruction.of_int 0x001110a3l)
    (RawInstruction.sh x1 (Option.value_exn (Immediate12.of_int32 1l)) x2)

let test_sw_x1_1_x1 () =
  Alcotest.(check raw_instruction)
    "sw x1, 1(x1)"
    (RawInstruction.of_int 0x0010a0a3l)
    (RawInstruction.sw x1 (Option.value_exn (Immediate12.of_int32 1l)) x1)

let test_sw_x1_1_x2 () =
  Alcotest.(check raw_instruction)
    "sw x1, 1(x2)"
    (RawInstruction.of_int 0x001120a3l)
    (RawInstruction.sw x1 (Option.value_exn (Immediate12.of_int32 1l)) x2)

let test_sw_x1_222222_x2 () =
  Alcotest.(check raw_instruction)
    "sw x1, 222(x2)"
    (RawInstruction.of_int 0x0c112f23l)
    (RawInstruction.sw x1 (Option.value_exn (Immediate12.of_int32 222l)) x2)

let test_beq_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "beq x1, x2, x3"
    (RawInstruction.of_int 0x00208163l)
    (RawInstruction.beq x1 x2 (Option.value_exn (Immediate12.of_int32 3l)))

let test_bne_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "bne x1, x2, x3"
    (RawInstruction.of_int 0x00209163l)
    (RawInstruction.bne x1 x2 (Option.value_exn (Immediate12.of_int32 3l)))

let test_blt_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "blt x1, x2, x3"
    (RawInstruction.of_int 0x0020c163l)
    (RawInstruction.blt x1 x2 (Option.value_exn (Immediate12.of_int32 3l)))

let test_bltu_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "bltu x1, x2, x3"
    (RawInstruction.of_int 0x0020e163l)
    (RawInstruction.bltu x1 x2 (Option.value_exn (Immediate12.of_int32 3l)))

let test_bge_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "bge x1, x2, x3"
    (RawInstruction.of_int 0x0020d163l)
    (RawInstruction.bge x1 x2 (Option.value_exn (Immediate12.of_int32 3l)))

let test_bgeu_x1_x2_x3 () =
  Alcotest.(check raw_instruction)
    "bgeu x1, x2, x3"
    (RawInstruction.of_int 0x0020f163l)
    (RawInstruction.bgeu x1 x2 (Option.value_exn (Immediate12.of_int32 3l)))

let test_jal_x1_1 () =
  Alcotest.(check raw_instruction)
    "jal x1, 1"
    (RawInstruction.of_int 0x000000efl)
    (RawInstruction.jal x1 (Option.value_exn (Immediate20.of_int32 1l)))

let test_jal_x0_imm_neg () =
  Alcotest.(check raw_instruction)
    "jal x0, -42"
    (RawInstruction.of_int 0xfd7ff06fl)
    (RawInstruction.jal x0
       (Option.value_exn (Immediate20.of_int32 (Int32.neg 42l))))

let test_jal_x1_2 () =
  Alcotest.(check raw_instruction)
    "jal x1, 2"
    (RawInstruction.of_int 0x002000efl)
    (RawInstruction.jal x1 (Option.value_exn (Immediate20.of_int32 2l)))

let test_jalr_x1_x2_1 () =
  Alcotest.(check raw_instruction)
    "jalr x1, 1(x2)"
    (RawInstruction.of_int 0x001100e7l)
    (RawInstruction.jalr x1 x2 (Option.value_exn (Immediate12.of_int32 1l)))

let test_ecall () =
  Alcotest.(check raw_instruction)
    "ecall"
    (RawInstruction.of_int 0x00000073l)
    RawInstruction.ecall

let test_ebreak () =
  Alcotest.(check raw_instruction)
    "ebreak"
    (RawInstruction.of_int 0x00100073l)
    RawInstruction.ebreak

let test_fence_i_o () =
  Alcotest.(check raw_instruction)
    "fence i, o"
    (RawInstruction.of_int 0x0840000fl)
    (RawInstruction.fence i o)

let suite =
  [
    ("Test add instruction", `Quick, test_add_x0_x0_x0);
    ("Test add instruction", `Quick, test_add_x1_x2_x3);
    ("Test addi instruction", `Quick, test_addi_x1_x2_x3);
    ("Test addi instruction negative immediate", `Quick, test_addi_x0_x0_imm_neg);
    ("Test sub instruction", `Quick, test_sub_x1_x2_x3);
    ("Test sll instruction", `Quick, test_sll_x1_x2_x3);
    ("Test srl instruction", `Quick, test_srl_x1_x2_x3);
    ("Test xor instruction", `Quick, test_xor_x1_x2_x3);
    ("Test or instruction", `Quick, test_or_x1_x2_x3);
    ("Test and instruction", `Quick, test_and_x1_x2_x3);
    ("Test slt instruction", `Quick, test_slt_x1_x2_x3);
    ("Test sltu instruction", `Quick, test_sltu_x1_x2_x3);
    ("Test sra instruction", `Quick, test_sra_x1_x2_x3);
    ("Test lui instruction", `Quick, test_lui_x1_1);
    ("Test lui instruction negative immediate", `Quick, test_lui_x0_imm_neg);
    ("Test auipc instruction", `Quick, test_auipc_x1_1);
    ("Test auipc instruction negative immediate", `Quick, test_auipc_x0_imm_neg);
    ("Test lw instruction", `Quick, test_lw_x1_1_x2);
    ("Test sw instruction", `Quick, test_sw_x1_1_x1);
    ("Test sw instruction", `Quick, test_sw_x1_1_x2);
    ("Test sw instruction", `Quick, test_sw_x1_222222_x2);
    ("Test beq instruction", `Quick, test_beq_x1_x2_x3);
    ("Test bne instruction", `Quick, test_bne_x1_x2_x3);
    ("Test blt instruction", `Quick, test_blt_x1_x2_x3);
    ("Test bltu instruction", `Quick, test_bltu_x1_x2_x3);
    ("Test bge instruction", `Quick, test_bge_x1_x2_x3);
    ("Test bgeu instruction", `Quick, test_bgeu_x1_x2_x3);
    ("Test jal instruction", `Quick, test_jal_x1_1);
    ("Test jal instruction", `Quick, test_jal_x1_2);
    ("Test jal instruction negative immediate", `Quick, test_jal_x0_imm_neg);
    ("Test jalr instruction", `Quick, test_jalr_x1_x2_1);
    ("Test ecall instruction", `Quick, test_ecall);
    ("Test ebreak instruction", `Quick, test_ebreak);
    ("Test fence instruction", `Quick, test_fence_i_o);
    ("Test lb instruction", `Quick, test_lb_x1_1_x2);
    ("Test lh instruction", `Quick, test_lh_x1_1_x2);
    ("Test lbu instruction", `Quick, test_lbu_x1_1_x2);
    ("Test lhu instruction", `Quick, test_lhu_x1_1_x2);
    ("Test sb instruction", `Quick, test_sb_x1_1_x2);
    ("Test sh instruction", `Quick, test_sh_x1_1_x2);
  ]
