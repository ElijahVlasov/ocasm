type t =
  | Unknown
  | Obscure
  | M68k
  | Vax
  | Or1k
  | Sparc
  | Spu
  | Mips
  | I386
  | Iamcu
  | Romp
  | Convex
  | M98k
  | Pyramid
  | H8300
  | Pdp11
  | Powerpc
  | Rs6000
  | Hppa
  | D10v
  | D30v
  | Dlx
  | M68hc11
  | M68hc12
  | M9s12x
  | M9s12xg
  | S12z
  | Z8k
  | Sh
  | Alpha
  | Arm
  | Nds32
  | Ns32k
  | Tic30
  | Tic4x
  | Tic54x
  | Tic6x
  | V850
  | V850_rh850
  | Arc
  | M32c
  | M32r
  | Mn10200
  | Mn10300
  | Fr30
  | Frv
  | Moxie
  | Ft32
  | Mcore
  | Mep
  | Metag
  | Ia64
  | Ip2k
  | Iq2000
  | Bpf
  | Epiphany
  | Mt
  | Pj
  | Avr
  | Bfin
  | Cr16
  | Crx
  | Cris
  | Riscv
  | Rl78
  | Rx
  | S390
  | Score
  | Mmix
  | Xstormy16
  | Msp430
  | Xgate
  | Xtensa
  | Z80
  | Lm32
  | Microblaze
  | Kvx
  | Tilepro
  | Tilegx
  | Aarch64
  | Visium
  | Wasm32
  | Pru
  | Nfp
  | Csky
  | Loongarch
  | Amdgcn
  | Last
[@@deriving eq]

val to_t_generated : t -> Types_generated.Arch.t

module Machine : sig
  type t

  val to_int32 : t -> int32
  val m68000 : t
  val m68008 : t
  val m68010 : t
  val m68020 : t
  val m68030 : t
  val m68040 : t
  val m68060 : t
  val cpu32 : t
  val fido : t
  val mcf_isa_a_nodiv : t
  val mcf_isa_a : t
  val mcf_isa_a_mac : t
  val mcf_isa_a_emac : t
  val mcf_isa_aplus : t
  val mcf_isa_aplus_mac : t
  val mcf_isa_aplus_emac : t
  val mcf_isa_b_nousp : t
  val mcf_isa_b_nousp_mac : t
  val mcf_isa_b_nousp_emac : t
  val mcf_isa_b : t
  val mcf_isa_b_mac : t
  val mcf_isa_b_emac : t
  val mcf_isa_b_float : t
  val mcf_isa_b_float_mac : t
  val mcf_isa_b_float_emac : t
  val mcf_isa_c : t
  val mcf_isa_c_mac : t
  val mcf_isa_c_emac : t
  val mcf_isa_c_nodiv : t
  val mcf_isa_c_nodiv_mac : t
  val mcf_isa_c_nodiv_emac : t
  val or1k : t
  val or1knd : t
  val sparc : t
  val sparc_sparclet : t
  val sparc_sparclite : t
  val sparc_v8plus : t
  val sparc_sparclite_le : t
  val sparc_v9 : t
  val spu : t
  val mips3000 : t
  val mips3900 : t
  val mips4000 : t
  val mips4010 : t
  val mips4100 : t
  val mips4111 : t
  val mips4120 : t
  val mips4300 : t
  val mips4400 : t
  val mips4600 : t
  val mips4650 : t
  val mips5000 : t
  val mips5400 : t
  val mips5500 : t
  val mips5900 : t
  val mips6000 : t
  val mips7000 : t
  val mips8000 : t
  val mips9000 : t
  val mips10000 : t
  val mips12000 : t
  val mips14000 : t
  val mips16000 : t
  val mips16 : t
  val mips5 : t
  val mips_loongson_2e : t
  val mips_loongson_2f : t
  val mips_gs464 : t
  val mips_gs464e : t
  val mips_gs264e : t
  val mips_octeon : t
  val mips_octeonp : t
  val mips_octeon2 : t
  val mips_octeon3 : t
  val mipsisa32 : t
  val mipsisa32r2 : t
  val mipsisa32r3 : t
  val mipsisa32r5 : t
  val mipsisa32r6 : t
  val mipsisa64 : t
  val mipsisa64r2 : t
  val mipsisa64r3 : t
  val mipsisa64r5 : t
  val mipsisa64r6 : t
  val mips_micromips : t
  val h8300 : t
  val h8300h : t
  val h8300s : t
  val h8300hn : t
  val h8300sn : t
  val h8300sx : t
  val h8300sxn : t
  val ppc : t
  val ppc64 : t
  val ppc_403 : t
  val ppc_403gc : t
  val ppc_405 : t
  val ppc_505 : t
  val ppc_601 : t
  val ppc_602 : t
  val ppc_603 : t
  val ppc_ec603e : t
  val ppc_604 : t
  val ppc_620 : t
  val ppc_630 : t
  val ppc_750 : t
  val ppc_860 : t
  val ppc_a35 : t
  val ppc_rs64ii : t
  val ppc_rs64iii : t
  val ppc_7400 : t
  val ppc_e500 : t
  val ppc_e500mc : t
  val ppc_e500mc64 : t
  val ppc_e5500 : t
  val ppc_e6500 : t
  val ppc_titan : t
  val ppc_vle : t
  val rs6k : t
  val rs6k_rs1 : t
  val rs6k_rsc : t
  val rs6k_rs2 : t
  val hppa10 : t
  val hppa11 : t
  val hppa20 : t
  val hppa20w : t
  val d10v : t
  val d10v_ts2 : t
  val d10v_ts3 : t
  val m6812_default : t
  val m6812 : t
  val m6812s : t
  val s12z_default : t
  val z8001 : t
  val z8002 : t
  val sh : t
  val arm_unknown : t
  val arm_2 : t
  val arm_2a : t
  val arm_3 : t
  val arm_3M : t
  val arm_4 : t
  val arm_4T : t
  val arm_5 : t
  val arm_5T : t
  val arm_5TE : t
  val arm_XScale : t
  val arm_ep9312 : t
  val arm_iWMMXt : t
  val arm_iWMMXt2 : t
  val arm_5TEJ : t
  val arm_6 : t
  val arm_6KZ : t
  val arm_6T2 : t
  val arm_6K : t
  val arm_7 : t
  val arm_6M : t
  val arm_6SM : t
  val arm_7EM : t
  val arm_8 : t
  val arm_8R : t
  val arm_8M_BASE : t
  val arm_8M_MAIN : t
  val arm_8_1M_MAIN : t
  val arm_9 : t
  val n1 : t
  val n1h : t
  val n1h_v2 : t
  val n1h_v3 : t
  val n1h_v3m : t
  val tic3x : t
  val tic4x : t
  val v850 : t
  val arc_a4 : t
  val arc_a5 : t
  val arc_arc600 : t
  val arc_arc601 : t
  val arc_arc700 : t
  val arc_arcv2 : t
  val mn10300 : t
  val am33 : t
  val am33_2 : t
  val frv : t
  val frvsimple : t
  val fr300 : t
  val fr400 : t
  val fr450 : t
  val fr500 : t
  val fr550 : t
  val moxie : t
  val ft32 : t
  val ft32b : t
  val mep : t
  val metag : t
  val ia64_elf64 : t
  val ia64_elf32 : t
  val ip2022 : t
  val ip2022ext : t
  val iq2000 : t
  val iq10 : t
  val bpf : t
  val xbpf : t
  val epiphany16 : t
  val epiphany32 : t
  val ms1 : t
  val mrisc2 : t
  val ms2 : t
  val avr1 : t
  val avr2 : t
  val avr25 : t
  val avr3 : t
  val avr31 : t
  val avr35 : t
  val avr4 : t
  val avr5 : t
  val avr51 : t
  val avr6 : t
  val avrtiny : t
  val avrxmega1 : t
  val avrxmega2 : t
  val avrxmega3 : t
  val avrxmega4 : t
  val avrxmega5 : t
  val avrxmega6 : t
  val avrxmega7 : t
  val bfin : t
  val cr16 : t
  val crx : t
  val cris_v0_v10 : t
  val cris_v32 : t
  val cris_v10_v32 : t
  val riscv32 : t
  val riscv64 : t
  val s390_31 : t
  val s390_64 : t
  val score3 : t
  val score7 : t
  val xstormy16 : t
  val msp11 : t
  val msp110 : t
  val msp12 : t
  val msp13 : t
  val msp14 : t
  val msp15 : t
  val msp16 : t
  val msp20 : t
  val msp21 : t
  val msp22 : t
  val msp23 : t
  val msp24 : t
  val msp26 : t
  val msp31 : t
  val msp32 : t
  val msp33 : t
  val msp41 : t
  val msp42 : t
  val msp43 : t
  val msp44 : t
  val msp430x : t
  val msp46 : t
  val msp47 : t
  val msp54 : t
  val xgate : t
  val xtensa : t
  val z80strict : t
  val z180 : t
  val z80 : t
  val ez80_z80 : t
  val ez80_adl : t
  val z80n : t
  val z80full : t
  val gbz80 : t
  val r800 : t
  val lm32 : t
  val kv3_unknown : t
  val kv3_1 : t
  val kv3_1_64 : t
  val kv3_1_usr : t
  val kv3_2 : t
  val kv3_2_64 : t
  val kv3_2_usr : t
  val kv4_1 : t
  val kv4_1_64 : t
  val kv4_1_usr : t
  val tilepro : t
  val tilegx : t
  val tilegx32 : t
  val aarch64 : t
  val aarch64_8R : t
  val aarch64_ilp32 : t
  val aarch64_llp64 : t
  val visium : t
  val wasm32 : t
  val pru : t
  val ck_unknown : t
  val ck510 : t
  val ck610 : t
  val ck801 : t
  val ck802 : t
  val ck803 : t
  val ck807 : t
  val ck810 : t
  val ck860 : t
  val loongarch32 : t
  val loongarch64 : t
end
