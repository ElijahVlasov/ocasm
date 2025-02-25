open Register
open Opcode
open Immediate
open Fence_type

type instruction_type =
  | R_type of {
      rd : register;
      rs1 : register;
      rs2 : register;
      funct3 : funct3;
      funct7 : funct7;
    }
  | I_type of {
      opcode : opcode option;
      rd : register;
      rs1 : register;
      imm : imm12;
      funct3 : funct3;
    }
  | S_type of {
      opcode : opcode;
      rs1 : register;
      rs2 : register;
      imm : imm12;
      funct3 : funct3;
    }
  | B_type of { rs1 : register; rs2 : register; imm : imm12; funct3 : funct3 }
  | U_type of { opcode : opcode; rd : register; imm : imm20 }
  | J_type of { opcode : opcode; rd : register; imm : imm20 }
  | F_type of {
      opcode : opcode;
      funct3 : funct3;
      pre : fence_type;
      succ : fence_type;
    }

module InstructionBuilder = struct
  let opcode x = Opcode.to_int32 x

  module BitLengths = struct
    let opcode_bit_length = 7
    let reg_bit_length = 5
    let funct3_bit_length = 3
    let fence_bit_length = 4
  end

  module Positions = struct
    let opcode = 0
    let rd = opcode + BitLengths.opcode_bit_length
    let funct3 = rd + BitLengths.reg_bit_length
    let rs1 = funct3 + BitLengths.funct3_bit_length
    let rs2 = rs1 + BitLengths.reg_bit_length
    let imm_11_0 = rs2
    let funct7 = rs2 + BitLengths.reg_bit_length
    let imm_11_5 = funct7
    let imm_4_0 = rd
    let imm_31_12 = funct3
  end

  let ( >> ) = Int32.shift_right_logical
  let ( << ) = Int32.shift_left
  let ( || ) = Int32.logor
  let ( && ) = Int32.logand
  let rd reg = Register.to_int32 reg << Positions.rd
  let rs1 reg = Register.to_int32 reg << Positions.rs1
  let rs2 reg = Register.to_int32 reg << Positions.rs2
  let funct3 x = Funct3.to_int32 x
  let funct7 x = Funct7.to_int32 x

  let imm_11_0 imm =
    let imm = Immediate12.to_int32 imm in
    imm << Positions.imm_11_0

  let imm_11_5 imm =
    let imm = Immediate12.to_int32 imm in
    (imm && 0b1111111100000l) << Positions.imm_11_5 - 5

  let imm_4_0 imm =
    let imm = Immediate12.to_int32 imm in
    (imm && 0b11111l) << Positions.imm_4_0

  let imm_31_12 imm =
    let imm = Immediate20.to_int32 imm in
    imm << Positions.imm_31_12

  let imm_j_type imm =
    let imm = Immediate20.to_int32 imm in
    let offset = Positions.funct3 in
    let imm_19_12 = imm >> 12 && 0b11111111l << offset in
    let offset = offset + 8 in
    let imm_11 = imm >> 11 && 0b1l << offset in
    let offset = offset + 1 in
    let imm_10_1 = (imm >> 1 && 0b1111111111l) << offset in
    let offset = offset + 10 in
    let imm_20 = (imm >> 20 && 0b1l) << offset in
    imm_19_12 || imm_11 || imm_10_1 || imm_20

  let imm_b_type imm =
    let imm = Immediate12.to_int32 imm in
    let imm11 = (imm && 0b100000000000l) >> 4 in
    let imm4_1 = (imm && 0b11110l) << 7 in
    let imm10_5 = (imm && 0b11111100000l) << 20 in
    let imm12 = (imm && 0b1000000000000l) << 19 in
    imm11 || imm4_1 || imm10_5 || imm12

  let fence pre succ =
    Fence_type.to_int32 pre << 24 || Fence_type.to_int32 succ << 20
end

let build_instruction =
  let ( || ) = Int32.logor in
  let module B = InstructionBuilder in
  function
  | R_type { rd; rs1; rs2; funct3; funct7 } ->
      B.opcode Opcodes.arith_reg_reg_opcode
      || B.rd rd || B.rs1 rs1 || B.rs2 rs2 || B.funct3 funct3 || B.funct7 funct7
  | I_type { opcode; rd; rs1; imm; funct3 } ->
      B.opcode (Option.value ~default:Opcodes.arith_reg_imm_opcode opcode)
      || B.rd rd || B.rs1 rs1 || B.imm_11_0 imm || B.funct3 funct3
      || B.opcode (Option.value ~default:Opcodes.arith_reg_imm_opcode opcode)
  | S_type { opcode; rs1; rs2; imm; funct3 } ->
      B.opcode opcode || B.rs1 rs1 || B.rs2 rs2 || B.imm_4_0 imm
      || B.imm_11_5 imm || B.funct3 funct3
  | B_type { rs1; rs2; imm; funct3 } ->
      B.opcode Opcodes.branch_opcode
      || B.rs1 rs1 || B.rs2 rs2 || B.imm_b_type imm || B.funct3 funct3
  | U_type { opcode; rd; imm } -> B.opcode opcode || B.rd rd || B.imm_31_12 imm
  | J_type { opcode; rd; imm } -> B.opcode opcode || B.rd rd || B.imm_j_type imm
  | F_type { opcode; funct3; pre; succ } ->
      B.opcode opcode || B.funct3 funct3 || B.fence pre succ
