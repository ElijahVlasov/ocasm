module Opcode : sig
  type t

  val of_int32 : int32 -> t
  val to_int32 : t -> int32
end = struct
  type t = int32

  let of_int32 x = Int32.logand x 0b1111111l
  let to_int32 x = x
end

type opcode = Opcode.t

module Opcodes = struct
  let arith_reg_imm_opcode = Opcode.of_int32 0b001_0011l
  let arith_reg_reg_opcode = Opcode.of_int32 0b011_0011l
  let lui_opcode = Opcode.of_int32 0b011_0111l
  let auipc_opcode = Opcode.of_int32 0b001_0111l
  let branch_opcode = Opcode.of_int32 0b110_0011l
  let jal_opcode = Opcode.of_int32 0b110_1111l
  let jalr_opcode = Opcode.of_int32 0b110_0111l
  let system_opcode = Opcode.of_int32 0b111_0011l
  let load_opcode = Opcode.of_int32 0b000_0011l
  let store_opcode = Opcode.of_int32 0b010_0011l
  let fence_opcode = Opcode.of_int32 0b000_1111l
end

module Funct3 : sig
  type t

  val of_int32 : int32 -> t
  val to_int32 : t -> int32
end = struct
  type t = int32

  let of_int32 x = Int32.shift_left (Int32.logand x 0b111l) 12
  let to_int32 x = x
end

type funct3 = Funct3.t

module Funct3s = struct
  let add_sub = Funct3.of_int32 0b000l
  let xor = Funct3.of_int32 0b100l
  let or_ = Funct3.of_int32 0b110l
  let and_ = Funct3.of_int32 0b111l
  let sll = Funct3.of_int32 0b001l
  let srl = Funct3.of_int32 0b101l
  let slt = Funct3.of_int32 0b010l
  let sltu = Funct3.of_int32 0b011l
  let lb = Funct3.of_int32 0b000l
  let lh = Funct3.of_int32 0b001l
  let lw = Funct3.of_int32 0b010l
  let lbu = Funct3.of_int32 0b100l
  let lhu = Funct3.of_int32 0b101l
  let sb = Funct3.of_int32 0b000l
  let sh = Funct3.of_int32 0b001l
  let sw = Funct3.of_int32 0b010l
  let beq = Funct3.of_int32 0b000l
  let bne = Funct3.of_int32 0b001l
  let blt = Funct3.of_int32 0b100l
  let bge = Funct3.of_int32 0b101l
  let bltu = Funct3.of_int32 0b110l
  let bgeu = Funct3.of_int32 0b111l
  let jalr = Funct3.of_int32 0b000l
  let priv = Funct3.of_int32 0b000l
  let fence = Funct3.of_int32 0b000l
end

module Funct7 : sig
  type t

  val of_int32 : int32 -> t
  val to_int32 : t -> int32
end = struct
  type t = int32

  let of_int32 x = Int32.shift_left (Int32.logand x 0b1111111l) 25
  let to_int32 x = x
end

type funct7 = Funct7.t

module Funct7s = struct
  let zero = Funct7.of_int32 0b0000000l
  let sub_sra = Funct7.of_int32 0b0100000l
end
