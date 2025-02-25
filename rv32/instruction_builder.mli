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

val build_instruction : instruction_type -> int32
