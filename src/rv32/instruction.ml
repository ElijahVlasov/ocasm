open Fence_type
open Immediate
open Instruction_builder
open Opcode
open Register

type register_or_imm = Imm of imm12 | Reg of register [@@deriving eq, show]

module type INSTRUCTION = sig
  type t

  val to_int : t -> int32
  val of_int : int32 -> t
  val to_string : t -> string
  val eq_t : t -> t -> bool

  (* Integer register-register operations *)
  val add : register -> register -> register_or_imm -> t
  val sub : register -> register -> register -> t
  val sll : register -> register -> register_or_imm -> t
  val srl : register -> register -> register_or_imm -> t
  val and_ : register -> register -> register_or_imm -> t
  val xor : register -> register -> register_or_imm -> t
  val or_ : register -> register -> register_or_imm -> t
  val slt : register -> register -> register_or_imm -> t
  val sltu : register -> register -> register_or_imm -> t
  val sra : register -> register -> register_or_imm -> t
  val lui : register -> imm20 -> t
  val auipc : register -> imm20 -> t
  val lb : register -> imm12 -> register -> t
  val lh : register -> imm12 -> register -> t
  val lw : register -> imm12 -> register -> t
  val lbu : register -> imm12 -> register -> t
  val lhu : register -> imm12 -> register -> t
  val sb : register -> imm12 -> register -> t
  val sh : register -> imm12 -> register -> t
  val sw : register -> imm12 -> register -> t
  val beq : register -> register -> imm12 -> t
  val bne : register -> register -> imm12 -> t
  val blt : register -> register -> imm12 -> t
  val bltu : register -> register -> imm12 -> t
  val bge : register -> register -> imm12 -> t
  val bgeu : register -> register -> imm12 -> t
  val jal : register -> imm20 -> t
  val jalr : register -> register -> imm12 -> t
  val ecall : t
  val ebreak : t
  val fence : fence_type -> fence_type -> t
  val of_int : int32 -> t
end

module RawInstruction : INSTRUCTION = struct
  type t = int32

  let to_int x = x
  let of_int x = x
  let to_string x = Printf.sprintf "0x%lx" x
  let eq_t x y = x = y

  let r_or_i_instruction (dst : register) (src1 : register)
      (src2 : register_or_imm) ?(funct7 : funct7 option)
      ?(opcode : opcode option) (funct3 : funct3) : t =
    build_instruction
      (match src2 with
      | Imm imm -> I_type { rd = dst; rs1 = src1; imm; funct3; opcode }
      | Reg reg ->
          R_type
            {
              rd = dst;
              rs1 = src1;
              rs2 = reg;
              funct3;
              funct7 = Option.value ~default:Funct7s.zero funct7;
            })

  let add (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    r_or_i_instruction dst src1 src2 Funct3s.add_sub

  let sub (dst : register) (src1 : register) (src2 : register) : t =
    r_or_i_instruction dst src1 (Reg src2) ~funct7:Funct7s.sub_sra
      Funct3s.add_sub

  let sll (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    r_or_i_instruction dst src1 src2 ~funct7:Funct7s.zero Funct3s.sll

  let srl (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    r_or_i_instruction dst src1 src2 ~funct7:Funct7s.zero Funct3s.srl

  let and_ (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    r_or_i_instruction dst src1 src2 ~funct7:Funct7s.zero Funct3s.and_

  let xor (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    r_or_i_instruction dst src1 src2 ~funct7:Funct7s.zero Funct3s.xor

  let or_ (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    r_or_i_instruction dst src1 src2 Funct3s.or_

  let slt (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    r_or_i_instruction dst src1 src2 Funct3s.slt

  let sltu (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    r_or_i_instruction dst src1 src2 Funct3s.sltu

  let sra (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    r_or_i_instruction dst src1 src2 ~funct7:Funct7s.sub_sra Funct3s.srl

  let lui (dst : register) (imm : imm20) : t =
    build_instruction (U_type { opcode = Opcodes.lui_opcode; rd = dst; imm })

  let auipc (dst : register) (imm : imm20) : t =
    build_instruction (U_type { opcode = Opcodes.auipc_opcode; rd = dst; imm })

  let lb (dst : register) (offset : imm12) (base : register) : t =
    build_instruction
      (I_type
         {
           opcode = Some Opcodes.load_opcode;
           rd = dst;
           rs1 = base;
           imm = offset;
           funct3 = Funct3s.lb;
         })

  let lh (dst : register) (offset : imm12) (base : register) : t =
    build_instruction
      (I_type
         {
           opcode = Some Opcodes.load_opcode;
           rd = dst;
           rs1 = base;
           imm = offset;
           funct3 = Funct3s.lh;
         })

  let lw (dst : register) (offset : imm12) (base : register) : t =
    build_instruction
      (I_type
         {
           opcode = Some Opcodes.load_opcode;
           rd = dst;
           rs1 = base;
           imm = offset;
           funct3 = Funct3s.lw;
         })

  let lbu (dst : register) (offset : imm12) (base : register) : t =
    build_instruction
      (I_type
         {
           opcode = Some Opcodes.load_opcode;
           rd = dst;
           rs1 = base;
           imm = offset;
           funct3 = Funct3s.lbu;
         })

  let lhu (dst : register) (offset : imm12) (base : register) : t =
    build_instruction
      (I_type
         {
           opcode = Some Opcodes.load_opcode;
           rd = dst;
           rs1 = base;
           imm = offset;
           funct3 = Funct3s.lhu;
         })

  let sb (src : register) (offset : imm12) (base : register) : t =
    build_instruction
      (S_type
         {
           opcode = Opcodes.store_opcode;
           rs1 = base;
           rs2 = src;
           imm = offset;
           funct3 = Funct3s.sb;
         })

  let sh (src : register) (offset : imm12) (base : register) : t =
    build_instruction
      (S_type
         {
           opcode = Opcodes.store_opcode;
           rs1 = base;
           rs2 = src;
           imm = offset;
           funct3 = Funct3s.sh;
         })

  let sw (src : register) (offset : imm12) (base : register) : t =
    build_instruction
      (S_type
         {
           opcode = Opcodes.store_opcode;
           rs1 = base;
           rs2 = src;
           imm = offset;
           funct3 = Funct3s.sw;
         })

  let beq (src1 : register) (src2 : register) (offset : imm12) : t =
    build_instruction
      (B_type { rs1 = src1; rs2 = src2; imm = offset; funct3 = Funct3s.beq })

  let bne (src1 : register) (src2 : register) (offset : imm12) : t =
    build_instruction
      (B_type { rs1 = src1; rs2 = src2; imm = offset; funct3 = Funct3s.bne })

  let blt (src1 : register) (src2 : register) (offset : imm12) : t =
    build_instruction
      (B_type { rs1 = src1; rs2 = src2; imm = offset; funct3 = Funct3s.blt })

  let bltu (src1 : register) (src2 : register) (offset : imm12) : t =
    build_instruction
      (B_type { rs1 = src1; rs2 = src2; imm = offset; funct3 = Funct3s.bltu })

  let bge (src1 : register) (src2 : register) (offset : imm12) : t =
    build_instruction
      (B_type { rs1 = src1; rs2 = src2; imm = offset; funct3 = Funct3s.bge })

  let bgeu (src1 : register) (src2 : register) (offset : imm12) : t =
    build_instruction
      (B_type { rs1 = src1; rs2 = src2; imm = offset; funct3 = Funct3s.bgeu })

  let jal (dst : register) (imm : imm20) : t =
    build_instruction (J_type { opcode = Opcodes.jal_opcode; rd = dst; imm })

  let jalr (dst : register) (base : register) (offset : imm12) : t =
    build_instruction
      (I_type
         {
           opcode = Some Opcodes.jalr_opcode;
           rd = dst;
           rs1 = base;
           imm = offset;
           funct3 = Funct3s.jalr;
         })

  let ecall : t =
    build_instruction
      (I_type
         {
           rd = zero;
           rs1 = zero;
           imm = zero_imm12;
           funct3 = Funct3s.priv;
           opcode = Some Opcodes.system_opcode;
         })

  let ebreak : t =
    build_instruction
      (I_type
         {
           rd = zero;
           rs1 = zero;
           imm = one_imm12;
           funct3 = Funct3s.priv;
           opcode = Some Opcodes.system_opcode;
         })

  let fence (pre : fence_type) (succ : fence_type) : t =
    build_instruction
      (F_type
         { opcode = Opcodes.fence_opcode; funct3 = Funct3s.fence; pre; succ })
end

type structured_instruction =
  (* Integer register-register operations *)
  | Add of register * register * register_or_imm
  | Sub of register * register * register
  | Sll of register * register * register_or_imm
  | Srl of register * register * register_or_imm
  | And of register * register * register_or_imm
  | Xor of register * register * register_or_imm
  | Or of register * register * register_or_imm
  | Slt of register * register * register_or_imm
  | Sltu of register * register * register_or_imm
  | Sra of register * register * register_or_imm
  | Lui of register * imm20
  | Auipc of register * imm20
  | Lb of register * register * imm12 (* LB rd, offset(rs1) *)
  | Lh of register * register * imm12 (* LH rd, offset(rs1) *)
  | Lw of register * register * imm12 (* LW rd, offset(rs1) *)
  | Lbu of register * register * imm12 (* LBU rd, offset(rs1) *)
  | Lhu of register * register * imm12 (* LHU rd, offset(rs1) *)
  | Sb of register * register * imm12 (* SB rs2, offset(rs1) *)
  | Sh of register * register * imm12 (* SH rs2, offset(rs1) *)
  | Sw of register * register * imm12 (* SW rs2, offset(rs1) *)
  | Beq of register * register * imm12
  | Bne of register * register * imm12
  | Blt of register * register * imm12
  | Bltu of register * register * imm12
  | Bge of register * register * imm12
  | Bgeu of register * register * imm12
  | Jal of register * imm20 (* JAL rd, offset *)
  | Jalr of register * register * imm12 (* JALR rd, rs1, offset *)
  | Ecall
  | Ebreak
  | Fence of fence_type * fence_type
[@@deriving eq, show]

module StructuredInstruction : INSTRUCTION = struct
  type t = structured_instruction

  let to_int x : int32 = 0l
  let of_int x = Add (zero, zero, Imm zero_imm12)
  let to_string x = show_structured_instruction x
  let eq_t x y = x = y

  let add (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    Add (dst, src1, src2)

  let sub (dst : register) (src1 : register) (src2 : register) : t =
    Sub (dst, src1, src2)

  let sll (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    Sll (dst, src1, src2)

  let srl (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    Srl (dst, src1, src2)

  let and_ (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    And (dst, src1, src2)

  let xor (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    Xor (dst, src1, src2)

  let or_ (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    Or (dst, src1, src2)

  let slt (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    Slt (dst, src1, src2)

  let sltu (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    Sltu (dst, src1, src2)

  let sra (dst : register) (src1 : register) (src2 : register_or_imm) : t =
    Sra (dst, src1, src2)

  let lui (dst : register) (imm : imm20) : t = Lui (dst, imm)
  let auipc (dst : register) (imm : imm20) : t = Auipc (dst, imm)

  let lb (dst : register) (offset : imm12) (base : register) : t =
    Lb (dst, base, offset)

  let lh (dst : register) (offset : imm12) (base : register) : t =
    Lh (dst, base, offset)

  let lw (dst : register) (offset : imm12) (base : register) : t =
    Lw (dst, base, offset)

  let lbu (dst : register) (offset : imm12) (base : register) : t =
    Lbu (dst, base, offset)

  let lhu (dst : register) (offset : imm12) (base : register) : t =
    Lhu (dst, base, offset)

  let sb (src : register) (offset : imm12) (base : register) : t =
    Sb (src, base, offset)

  let sh (src : register) (offset : imm12) (base : register) : t =
    Sh (src, base, offset)

  let sw (src : register) (offset : imm12) (base : register) : t =
    Sw (src, base, offset)

  let beq (src1 : register) (src2 : register) (offset : imm12) : t =
    Beq (src1, src2, offset)

  let bne (src1 : register) (src2 : register) (offset : imm12) : t =
    Bne (src1, src2, offset)

  let blt (src1 : register) (src2 : register) (offset : imm12) : t =
    Blt (src1, src2, offset)

  let bltu (src1 : register) (src2 : register) (offset : imm12) : t =
    Bltu (src1, src2, offset)

  let bge (src1 : register) (src2 : register) (offset : imm12) : t =
    Bge (src1, src2, offset)

  let bgeu (src1 : register) (src2 : register) (offset : imm12) : t =
    Bgeu (src1, src2, offset)

  let jal (dst : register) (offset : imm20) : t = Jal (dst, offset)

  let jalr (dst : register) (base : register) (offset : imm12) : t =
    Jalr (dst, base, offset)

  let ecall : t = Ecall
  let ebreak : t = Ebreak
  let fence (a : fence_type) (b : fence_type) : t = Fence (a, b)
end

let ast_to_raw =
  let module I = RawInstruction in
  function
  | Add (dst, src1, src2) -> I.add dst src1 src2
  | Sub (dst, src1, src2) -> I.sub dst src1 src2
  | Sll (dst, src1, src2) -> I.sll dst src1 src2
  | Srl (dst, src1, src2) -> I.srl dst src1 src2
  | And (dst, src1, src2) -> I.and_ dst src1 src2
  | Xor (dst, src1, src2) -> I.xor dst src1 src2
  | Or (dst, src1, src2) -> I.or_ dst src1 src2
  | Slt (dst, src1, src2) -> I.slt dst src1 src2
  | Sltu (dst, src1, src2) -> I.sltu dst src1 src2
  | Sra (dst, src1, src2) -> I.sra dst src1 src2
  | Lui (dst, imm) -> I.lui dst imm
  | Auipc (dst, imm) -> I.auipc dst imm
  | Lb (dst, base, offset) -> I.lb dst offset base
  | Lh (dst, base, offset) -> I.lh dst offset base
  | Lw (dst, base, offset) -> I.lw dst offset base
  | Lbu (dst, base, offset) -> I.lbu dst offset base
  | Lhu (dst, base, offset) -> I.lhu dst offset base
  | Sb (src, base, offset) -> I.sb src offset base
  | Sh (src, base, offset) -> I.sh src offset base
  | Sw (src, base, offset) -> I.sw src offset base
  | Beq (src1, src2, offset) -> I.beq src1 src2 offset
  | Bne (src1, src2, offset) -> I.bne src1 src2 offset
  | Blt (src1, src2, offset) -> I.blt src1 src2 offset
  | Bltu (src1, src2, offset) -> I.bltu src1 src2 offset
  | Bge (src1, src2, offset) -> I.bge src1 src2 offset
  | Bgeu (src1, src2, offset) -> I.bgeu src1 src2 offset
  | Jal (dst, offset) -> I.jal dst offset
  | Jalr (dst, base, offset) -> I.jalr dst base offset
  | Ecall -> I.ecall
  | Ebreak -> I.ebreak
  | Fence (a, b) -> I.fence a b
