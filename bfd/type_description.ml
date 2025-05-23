open Ctypes

module Types (F : TYPE) = struct
  open F

  let bfd_arch_size = constant "BFD_ARCH_SIZE" size_t
  let bfd_init_magic = constant "BFD_INIT_MAGIC" size_t

  type bfdm
  type bfd = bfdm Ctypes.structure

  let bfd : bfd structure typ = structure "bfd"
  let bfd_filename = field bfd "filename" string
  let bfd_section_count = field bfd "section_count" int
  let bfd_error_no_error = constant "bfd_error_no_error" int64_t
  let bfd_error_system_call = constant "bfd_error_system_call" int64_t
  let bfd_error_invalid_target = constant "bfd_error_invalid_target" int64_t
  let bfd_error_wrong_format = constant "bfd_error_wrong_format" int64_t

  let bfd_error_wrong_object_format =
    constant "bfd_error_wrong_object_format" int64_t

  let bfd_error_invalid_operation =
    constant "bfd_error_invalid_operation" int64_t

  let bfd_error_no_memory = constant "bfd_error_no_memory" int64_t
  let bfd_error_no_symbols = constant "bfd_error_no_symbols" int64_t
  let bfd_error_no_armap = constant "bfd_error_no_armap" int64_t

  let bfd_error_no_more_archived_files =
    constant "bfd_error_no_more_archived_files" int64_t

  let bfd_error_malformed_archive =
    constant "bfd_error_malformed_archive" int64_t

  let bfd_error_missing_dso = constant "bfd_error_missing_dso" int64_t

  let bfd_error_file_not_recognized =
    constant "bfd_error_file_not_recognized" int64_t

  let bfd_error_file_ambiguously_recognized =
    constant "bfd_error_file_ambiguously_recognized" int64_t

  let bfd_error_no_contents = constant "bfd_error_no_contents" int64_t

  let bfd_error_nonrepresentable_section =
    constant "bfd_error_nonrepresentable_section" int64_t

  let bfd_error_no_debug_section = constant "bfd_error_no_debug_section" int64_t
  let bfd_error_bad_value = constant "bfd_error_bad_value" int64_t
  let bfd_error_file_truncated = constant "bfd_error_file_truncated" int64_t
  let bfd_error_file_too_big = constant "bfd_error_file_too_big" int64_t
  let bfd_error_sorry = constant "bfd_error_sorry" int64_t
  let bfd_error_on_input = constant "bfd_error_on_input" int64_t

  let bfd_error_invalid_error_code =
    constant "bfd_error_invalid_error_code" int64_t

  module Error = struct
    type t =
      | NoError
      | SystemCall
      | InvalidTarget
      | WrongFormat
      | WrongObjectFormat
      | InvalidOperation
      | NoMemory
      | NoSymbols
      | NoArmap
      | NoMoreArchivedFiles
      | MalformedArchive
      | MissingDso
      | FileNotRecognized
      | FileAmbiguouslyRecognized
      | NoContents
      | NonrepresentableSection
      | NoDebugSection
      | BadValue
      | FileTruncated
      | FileTooBig
      | Sorry
      | OnInput
      | InvalidErrorCode
    [@@deriving eq]
  end

  let bfd_error_type =
    enum "bfd_error"
      [
        (Error.NoError, bfd_error_no_error);
        (Error.SystemCall, bfd_error_system_call);
        (Error.InvalidTarget, bfd_error_invalid_target);
        (Error.WrongFormat, bfd_error_wrong_format);
        (Error.WrongObjectFormat, bfd_error_wrong_object_format);
        (Error.InvalidOperation, bfd_error_invalid_operation);
        (Error.NoMemory, bfd_error_no_memory);
        (Error.NoSymbols, bfd_error_no_symbols);
        (Error.NoArmap, bfd_error_no_armap);
        (Error.NoMoreArchivedFiles, bfd_error_no_more_archived_files);
        (Error.MalformedArchive, bfd_error_malformed_archive);
        (Error.MissingDso, bfd_error_missing_dso);
        (Error.FileNotRecognized, bfd_error_file_not_recognized);
        (Error.FileAmbiguouslyRecognized, bfd_error_file_ambiguously_recognized);
        (Error.NoContents, bfd_error_no_contents);
        (Error.NonrepresentableSection, bfd_error_nonrepresentable_section);
        (Error.NoDebugSection, bfd_error_no_debug_section);
        (Error.BadValue, bfd_error_bad_value);
        (Error.FileTruncated, bfd_error_file_truncated);
        (Error.FileTooBig, bfd_error_file_too_big);
        (Error.Sorry, bfd_error_sorry);
        (Error.OnInput, bfd_error_on_input);
        (Error.InvalidErrorCode, bfd_error_invalid_error_code);
      ]

  let bfd_object = constant "bfd_object" int64_t

  type asectionm
  type asection = asectionm Ctypes.structure

  let asection : asection structure typ = structure "asection"
  let sec_no_flags = constant "SEC_NO_FLAGS" int32_t
  let sec_alloc = constant "SEC_ALLOC" int32_t
  let sec_load = constant "SEC_LOAD" int32_t
  let sec_reloc = constant "SEC_RELOC" int32_t
  let sec_readonly = constant "SEC_READONLY" int32_t
  let sec_code = constant "SEC_CODE" int32_t
  let sec_data = constant "SEC_DATA" int32_t
  let sec_rom = constant "SEC_ROM" int32_t
  let sec_constructor = constant "SEC_CONSTRUCTOR" int32_t
  let sec_has_contents = constant "SEC_HAS_CONTENTS" int32_t
  let sec_never_load = constant "SEC_NEVER_LOAD" int32_t
  let sec_coff_shared_library = constant "SEC_COFF_SHARED_LIBRARY" int32_t
  let sec_is_common = constant "SEC_IS_COMMON" int32_t
  let sec_debugging = constant "SEC_DEBUGGING" int32_t
  let sec_in_memory = constant "SEC_IN_MEMORY" int32_t
  let sec_exclude = constant "SEC_EXCLUDE" int32_t
  let sec_sort_entries = constant "SEC_SORT_ENTRIES" int32_t
  let sec_link_once = constant "SEC_LINK_ONCE" int32_t
  let sec_link_duplicates = constant "SEC_LINK_DUPLICATES" int32_t

  let sec_link_duplicates_discard =
    constant "SEC_LINK_DUPLICATES_DISCARD" int32_t

  let sec_link_duplicates_one_only =
    constant "SEC_LINK_DUPLICATES_ONE_ONLY" int32_t

  let sec_link_duplicates_same_size =
    constant "SEC_LINK_DUPLICATES_SAME_SIZE" int32_t

  let sec_link_duplicates_same_contents =
    constant "SEC_LINK_DUPLICATES_SAME_CONTENTS" int32_t

  let sec_linker_created = constant "SEC_LINKER_CREATED" int32_t

  type asymbolm
  type asymbol = asymbolm Ctypes.structure

  let asymbol : asymbol structure typ = structure "bfd_symbol"
  let asym_name = field asymbol "name" string
  let asym_section = field asymbol "section" (ptr asection)
  let asym_flags = field asymbol "flags" int32_t
  let asym_value = field asymbol "value" int64_t
  let () = seal asymbol
  let bsf_no_flags = constant "BSF_NO_FLAGS" int32_t
  let bsf_local = constant "BSF_LOCAL" int32_t
  let bsf_global = constant "BSF_GLOBAL" int32_t
  let bsf_export = constant "BSF_EXPORT" int32_t
  let bsf_debugging = constant "BSF_DEBUGGING" int32_t
  let bsf_function = constant "BSF_FUNCTION" int32_t
  let bsf_keep = constant "BSF_KEEP" int32_t
  let bsf_elf_common = constant "BSF_ELF_COMMON" int32_t
  let bsf_weak = constant "BSF_WEAK" int32_t
  let bsf_section_sym = constant "BSF_SECTION_SYM" int32_t
  let bsf_old_common = constant "BSF_OLD_COMMON" int32_t
  let bsf_not_at_end = constant "BSF_NOT_AT_END" int32_t
  let bsf_constructor = constant "BSF_CONSTRUCTOR" int32_t
  let bsf_warning = constant "BSF_WARNING" int32_t
  let bsf_indirect = constant "BSF_INDIRECT" int32_t
  let bsf_file = constant "BSF_FILE" int32_t
  let bsf_dynamic = constant "BSF_DYNAMIC" int32_t
  let bsf_object = constant "BSF_OBJECT" int32_t
  let bsf_debugging_reloc = constant "BSF_DEBUGGING_RELOC" int32_t
  let bsf_thread_local = constant "BSF_THREAD_LOCAL" int32_t
  let bsf_relc = constant "BSF_RELC" int32_t
  let bsf_srelc = constant "BSF_SRELC" int32_t
  let bsf_synthetic = constant "BSF_SYNTHETIC" int32_t
  let bsf_gnu_indirect_function = constant "BSF_GNU_INDIRECT_FUNCTION" int32_t
  let bsf_gnu_unique = constant "BSF_GNU_UNIQUE" int32_t
  let bsf_section_sym_used = constant "BSF_SECTION_SYM_USED" int32_t

  type bfd_targetm
  type bfd_target = bfd_targetm Ctypes.structure

  let bfd_target : bfd_target structure typ = structure "bfd_target"

  let bfd_make_empty_symbol_t =
    static_funptr (ptr bfd @-> returning (ptr asymbol))

  let bfd_make_empty_symbol =
    field bfd_target "_bfd_make_empty_symbol" bfd_make_empty_symbol_t

  let xvec = field bfd "xvec" (ptr bfd_target)
  let () = seal bfd_target
  let () = seal bfd
  let bfd_arch_unknown = constant "bfd_arch_unknown" int64_t
  let bfd_arch_obscure = constant "bfd_arch_obscure" int64_t
  let bfd_arch_m68k = constant "bfd_arch_m68k" int64_t
  let bfd_arch_vax = constant "bfd_arch_vax" int64_t
  let bfd_arch_or1k = constant "bfd_arch_or1k" int64_t
  let bfd_arch_sparc = constant "bfd_arch_sparc" int64_t
  let bfd_arch_spu = constant "bfd_arch_spu" int64_t
  let bfd_arch_mips = constant "bfd_arch_mips" int64_t
  let bfd_arch_i386 = constant "bfd_arch_i386" int64_t
  let bfd_arch_iamcu = constant "bfd_arch_iamcu" int64_t
  let bfd_arch_romp = constant "bfd_arch_romp" int64_t
  let bfd_arch_convex = constant "bfd_arch_convex" int64_t
  let bfd_arch_m98k = constant "bfd_arch_m98k" int64_t
  let bfd_arch_pyramid = constant "bfd_arch_pyramid" int64_t
  let bfd_arch_h8300 = constant "bfd_arch_h8300" int64_t
  let bfd_arch_pdp11 = constant "bfd_arch_pdp11" int64_t
  let bfd_arch_powerpc = constant "bfd_arch_powerpc" int64_t
  let bfd_arch_rs6000 = constant "bfd_arch_rs6000" int64_t
  let bfd_arch_hppa = constant "bfd_arch_hppa" int64_t
  let bfd_arch_d10v = constant "bfd_arch_d10v" int64_t
  let bfd_arch_d30v = constant "bfd_arch_d30v" int64_t
  let bfd_arch_dlx = constant "bfd_arch_dlx" int64_t
  let bfd_arch_m68hc11 = constant "bfd_arch_m68hc11" int64_t
  let bfd_arch_m68hc12 = constant "bfd_arch_m68hc12" int64_t
  let bfd_arch_m9s12x = constant "bfd_arch_m9s12x" int64_t
  let bfd_arch_m9s12xg = constant "bfd_arch_m9s12xg" int64_t
  let bfd_arch_s12z = constant "bfd_arch_s12z" int64_t
  let bfd_arch_z8k = constant "bfd_arch_z8k" int64_t
  let bfd_arch_sh = constant "bfd_arch_sh" int64_t
  let bfd_arch_alpha = constant "bfd_arch_alpha" int64_t
  let bfd_arch_arm = constant "bfd_arch_arm" int64_t
  let bfd_arch_nds32 = constant "bfd_arch_nds32" int64_t
  let bfd_arch_ns32k = constant "bfd_arch_ns32k" int64_t
  let bfd_arch_tic30 = constant "bfd_arch_tic30" int64_t
  let bfd_arch_tic4x = constant "bfd_arch_tic4x" int64_t
  let bfd_arch_tic54x = constant "bfd_arch_tic54x" int64_t
  let bfd_arch_tic6x = constant "bfd_arch_tic6x" int64_t
  let bfd_arch_v850 = constant "bfd_arch_v850" int64_t
  let bfd_arch_v850_rh850 = constant "bfd_arch_v850_rh850" int64_t
  let bfd_arch_arc = constant "bfd_arch_arc" int64_t
  let bfd_arch_m32c = constant "bfd_arch_m32c" int64_t
  let bfd_arch_m32r = constant "bfd_arch_m32r" int64_t
  let bfd_arch_mn10200 = constant "bfd_arch_mn10200" int64_t
  let bfd_arch_mn10300 = constant "bfd_arch_mn10300" int64_t
  let bfd_arch_fr30 = constant "bfd_arch_fr30" int64_t
  let bfd_arch_frv = constant "bfd_arch_frv" int64_t
  let bfd_arch_moxie = constant "bfd_arch_moxie" int64_t
  let bfd_arch_ft32 = constant "bfd_arch_ft32" int64_t
  let bfd_arch_mcore = constant "bfd_arch_mcore" int64_t
  let bfd_arch_mep = constant "bfd_arch_mep" int64_t
  let bfd_arch_metag = constant "bfd_arch_metag" int64_t
  let bfd_arch_ia64 = constant "bfd_arch_ia64" int64_t
  let bfd_arch_ip2k = constant "bfd_arch_ip2k" int64_t
  let bfd_arch_iq2000 = constant "bfd_arch_iq2000" int64_t
  let bfd_arch_bpf = constant "bfd_arch_bpf" int64_t
  let bfd_arch_epiphany = constant "bfd_arch_epiphany" int64_t
  let bfd_arch_mt = constant "bfd_arch_mt" int64_t
  let bfd_arch_pj = constant "bfd_arch_pj" int64_t
  let bfd_arch_avr = constant "bfd_arch_avr" int64_t
  let bfd_arch_bfin = constant "bfd_arch_bfin" int64_t
  let bfd_arch_cr16 = constant "bfd_arch_cr16" int64_t
  let bfd_arch_crx = constant "bfd_arch_crx" int64_t
  let bfd_arch_cris = constant "bfd_arch_cris" int64_t
  let bfd_arch_riscv = constant "bfd_arch_riscv" int64_t
  let bfd_arch_rl78 = constant "bfd_arch_rl78" int64_t
  let bfd_arch_rx = constant "bfd_arch_rx" int64_t
  let bfd_arch_s390 = constant "bfd_arch_s390" int64_t
  let bfd_arch_score = constant "bfd_arch_score" int64_t
  let bfd_arch_mmix = constant "bfd_arch_mmix" int64_t
  let bfd_arch_xstormy16 = constant "bfd_arch_xstormy16" int64_t
  let bfd_arch_msp430 = constant "bfd_arch_msp430" int64_t
  let bfd_arch_xgate = constant "bfd_arch_xgate" int64_t
  let bfd_arch_xtensa = constant "bfd_arch_xtensa" int64_t
  let bfd_arch_z80 = constant "bfd_arch_z80" int64_t
  let bfd_arch_lm32 = constant "bfd_arch_lm32" int64_t
  let bfd_arch_microblaze = constant "bfd_arch_microblaze" int64_t
  let bfd_arch_kvx = constant "bfd_arch_kvx" int64_t
  let bfd_arch_tilepro = constant "bfd_arch_tilepro" int64_t
  let bfd_arch_tilegx = constant "bfd_arch_tilegx" int64_t
  let bfd_arch_aarch64 = constant "bfd_arch_aarch64" int64_t
  let bfd_arch_visium = constant "bfd_arch_visium" int64_t
  let bfd_arch_wasm32 = constant "bfd_arch_wasm32" int64_t
  let bfd_arch_pru = constant "bfd_arch_pru" int64_t
  let bfd_arch_nfp = constant "bfd_arch_nfp" int64_t
  let bfd_arch_csky = constant "bfd_arch_csky" int64_t
  let bfd_arch_loongarch = constant "bfd_arch_loongarch" int64_t
  let bfd_arch_amdgcn = constant "bfd_arch_amdgcn" int64_t
  let bfd_arch_last = constant "bfd_arch_last" int64_t

  module Arch = struct
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
  end

  let bfd_architecture_type =
    enum "bfd_architecture"
      [
        (Arch.Unknown, bfd_arch_unknown);
        (Arch.Obscure, bfd_arch_obscure);
        (Arch.M68k, bfd_arch_m68k);
        (Arch.Vax, bfd_arch_vax);
        (Arch.Or1k, bfd_arch_or1k);
        (Arch.Sparc, bfd_arch_sparc);
        (Arch.Spu, bfd_arch_spu);
        (Arch.Mips, bfd_arch_mips);
        (Arch.I386, bfd_arch_i386);
        (Arch.Iamcu, bfd_arch_iamcu);
        (Arch.Romp, bfd_arch_romp);
        (Arch.Convex, bfd_arch_convex);
        (Arch.M98k, bfd_arch_m98k);
        (Arch.Pyramid, bfd_arch_pyramid);
        (Arch.H8300, bfd_arch_h8300);
        (Arch.Pdp11, bfd_arch_pdp11);
        (Arch.Powerpc, bfd_arch_powerpc);
        (Arch.Rs6000, bfd_arch_rs6000);
        (Arch.Hppa, bfd_arch_hppa);
        (Arch.D10v, bfd_arch_d10v);
        (Arch.D30v, bfd_arch_d30v);
        (Arch.Dlx, bfd_arch_dlx);
        (Arch.M68hc11, bfd_arch_m68hc11);
        (Arch.M68hc12, bfd_arch_m68hc12);
        (Arch.M9s12x, bfd_arch_m9s12x);
        (Arch.M9s12xg, bfd_arch_m9s12xg);
        (Arch.S12z, bfd_arch_s12z);
        (Arch.Z8k, bfd_arch_z8k);
        (Arch.Sh, bfd_arch_sh);
        (Arch.Alpha, bfd_arch_alpha);
        (Arch.Arm, bfd_arch_arm);
        (Arch.Nds32, bfd_arch_nds32);
        (Arch.Ns32k, bfd_arch_ns32k);
        (Arch.Tic30, bfd_arch_tic30);
        (Arch.Tic4x, bfd_arch_tic4x);
        (Arch.Tic54x, bfd_arch_tic54x);
        (Arch.Tic6x, bfd_arch_tic6x);
        (Arch.V850, bfd_arch_v850);
        (Arch.V850_rh850, bfd_arch_v850_rh850);
        (Arch.Arc, bfd_arch_arc);
        (Arch.M32c, bfd_arch_m32c);
        (Arch.M32r, bfd_arch_m32r);
        (Arch.Mn10200, bfd_arch_mn10200);
        (Arch.Mn10300, bfd_arch_mn10300);
        (Arch.Fr30, bfd_arch_fr30);
        (Arch.Frv, bfd_arch_frv);
        (Arch.Moxie, bfd_arch_moxie);
        (Arch.Ft32, bfd_arch_ft32);
        (Arch.Mcore, bfd_arch_mcore);
        (Arch.Mep, bfd_arch_mep);
        (Arch.Metag, bfd_arch_metag);
        (Arch.Ia64, bfd_arch_ia64);
        (Arch.Ip2k, bfd_arch_ip2k);
        (Arch.Iq2000, bfd_arch_iq2000);
        (Arch.Bpf, bfd_arch_bpf);
        (Arch.Epiphany, bfd_arch_epiphany);
        (Arch.Mt, bfd_arch_mt);
        (Arch.Pj, bfd_arch_pj);
        (Arch.Avr, bfd_arch_avr);
        (Arch.Bfin, bfd_arch_bfin);
        (Arch.Cr16, bfd_arch_cr16);
        (Arch.Crx, bfd_arch_crx);
        (Arch.Cris, bfd_arch_cris);
        (Arch.Riscv, bfd_arch_riscv);
        (Arch.Rl78, bfd_arch_rl78);
        (Arch.Rx, bfd_arch_rx);
        (Arch.S390, bfd_arch_s390);
        (Arch.Score, bfd_arch_score);
        (Arch.Mmix, bfd_arch_mmix);
        (Arch.Xstormy16, bfd_arch_xstormy16);
        (Arch.Msp430, bfd_arch_msp430);
        (Arch.Xgate, bfd_arch_xgate);
        (Arch.Xtensa, bfd_arch_xtensa);
        (Arch.Z80, bfd_arch_z80);
        (Arch.Lm32, bfd_arch_lm32);
        (Arch.Microblaze, bfd_arch_microblaze);
        (Arch.Kvx, bfd_arch_kvx);
        (Arch.Tilepro, bfd_arch_tilepro);
        (Arch.Tilegx, bfd_arch_tilegx);
        (Arch.Aarch64, bfd_arch_aarch64);
        (Arch.Visium, bfd_arch_visium);
        (Arch.Wasm32, bfd_arch_wasm32);
        (Arch.Pru, bfd_arch_pru);
        (Arch.Nfp, bfd_arch_nfp);
        (Arch.Csky, bfd_arch_csky);
        (Arch.Loongarch, bfd_arch_loongarch);
        (Arch.Amdgcn, bfd_arch_amdgcn);
        (Arch.Last, bfd_arch_last);
      ]

  let bfd_mach_m68000 = constant "bfd_mach_m68000" int32_t
  let bfd_mach_m68008 = constant "bfd_mach_m68008" int32_t
  let bfd_mach_m68010 = constant "bfd_mach_m68010" int32_t
  let bfd_mach_m68020 = constant "bfd_mach_m68020" int32_t
  let bfd_mach_m68030 = constant "bfd_mach_m68030" int32_t
  let bfd_mach_m68040 = constant "bfd_mach_m68040" int32_t
  let bfd_mach_m68060 = constant "bfd_mach_m68060" int32_t
  let bfd_mach_cpu32 = constant "bfd_mach_cpu32" int32_t
  let bfd_mach_fido = constant "bfd_mach_fido" int32_t
  let bfd_mach_mcf_isa_a_nodiv = constant "bfd_mach_mcf_isa_a_nodiv" int32_t
  let bfd_mach_mcf_isa_a = constant "bfd_mach_mcf_isa_a" int32_t
  let bfd_mach_mcf_isa_a_mac = constant "bfd_mach_mcf_isa_a_mac" int32_t
  let bfd_mach_mcf_isa_a_emac = constant "bfd_mach_mcf_isa_a_emac" int32_t
  let bfd_mach_mcf_isa_aplus = constant "bfd_mach_mcf_isa_aplus" int32_t
  let bfd_mach_mcf_isa_aplus_mac = constant "bfd_mach_mcf_isa_aplus_mac" int32_t

  let bfd_mach_mcf_isa_aplus_emac =
    constant "bfd_mach_mcf_isa_aplus_emac" int32_t

  let bfd_mach_mcf_isa_b_nousp = constant "bfd_mach_mcf_isa_b_nousp" int32_t

  let bfd_mach_mcf_isa_b_nousp_mac =
    constant "bfd_mach_mcf_isa_b_nousp_mac" int32_t

  let bfd_mach_mcf_isa_b_nousp_emac =
    constant "bfd_mach_mcf_isa_b_nousp_emac" int32_t

  let bfd_mach_mcf_isa_b = constant "bfd_mach_mcf_isa_b" int32_t
  let bfd_mach_mcf_isa_b_mac = constant "bfd_mach_mcf_isa_b_mac" int32_t
  let bfd_mach_mcf_isa_b_emac = constant "bfd_mach_mcf_isa_b_emac" int32_t
  let bfd_mach_mcf_isa_b_float = constant "bfd_mach_mcf_isa_b_float" int32_t

  let bfd_mach_mcf_isa_b_float_mac =
    constant "bfd_mach_mcf_isa_b_float_mac" int32_t

  let bfd_mach_mcf_isa_b_float_emac =
    constant "bfd_mach_mcf_isa_b_float_emac" int32_t

  let bfd_mach_mcf_isa_c = constant "bfd_mach_mcf_isa_c" int32_t
  let bfd_mach_mcf_isa_c_mac = constant "bfd_mach_mcf_isa_c_mac" int32_t
  let bfd_mach_mcf_isa_c_emac = constant "bfd_mach_mcf_isa_c_emac" int32_t
  let bfd_mach_mcf_isa_c_nodiv = constant "bfd_mach_mcf_isa_c_nodiv" int32_t

  let bfd_mach_mcf_isa_c_nodiv_mac =
    constant "bfd_mach_mcf_isa_c_nodiv_mac" int32_t

  let bfd_mach_mcf_isa_c_nodiv_emac =
    constant "bfd_mach_mcf_isa_c_nodiv_emac" int32_t

  let bfd_mach_or1k = constant "bfd_mach_or1k" int32_t
  let bfd_mach_or1knd = constant "bfd_mach_or1knd" int32_t
  let bfd_mach_sparc = constant "bfd_mach_sparc" int32_t
  let bfd_mach_sparc_sparclet = constant "bfd_mach_sparc_sparclet" int32_t
  let bfd_mach_sparc_sparclite = constant "bfd_mach_sparc_sparclite" int32_t
  let bfd_mach_sparc_v8plus = constant "bfd_mach_sparc_v8plus" int32_t

  let bfd_mach_sparc_sparclite_le =
    constant "bfd_mach_sparc_sparclite_le" int32_t

  let bfd_mach_sparc_v9 = constant "bfd_mach_sparc_v9" int32_t
  let bfd_mach_spu = constant "bfd_mach_spu" int32_t
  let bfd_mach_mips3000 = constant "bfd_mach_mips3000" int32_t
  let bfd_mach_mips3900 = constant "bfd_mach_mips3900" int32_t
  let bfd_mach_mips4000 = constant "bfd_mach_mips4000" int32_t
  let bfd_mach_mips4010 = constant "bfd_mach_mips4010" int32_t
  let bfd_mach_mips4100 = constant "bfd_mach_mips4100" int32_t
  let bfd_mach_mips4111 = constant "bfd_mach_mips4111" int32_t
  let bfd_mach_mips4120 = constant "bfd_mach_mips4120" int32_t
  let bfd_mach_mips4300 = constant "bfd_mach_mips4300" int32_t
  let bfd_mach_mips4400 = constant "bfd_mach_mips4400" int32_t
  let bfd_mach_mips4600 = constant "bfd_mach_mips4600" int32_t
  let bfd_mach_mips4650 = constant "bfd_mach_mips4650" int32_t
  let bfd_mach_mips5000 = constant "bfd_mach_mips5000" int32_t
  let bfd_mach_mips5400 = constant "bfd_mach_mips5400" int32_t
  let bfd_mach_mips5500 = constant "bfd_mach_mips5500" int32_t
  let bfd_mach_mips5900 = constant "bfd_mach_mips5900" int32_t
  let bfd_mach_mips6000 = constant "bfd_mach_mips6000" int32_t
  let bfd_mach_mips7000 = constant "bfd_mach_mips7000" int32_t
  let bfd_mach_mips8000 = constant "bfd_mach_mips8000" int32_t
  let bfd_mach_mips9000 = constant "bfd_mach_mips9000" int32_t
  let bfd_mach_mips10000 = constant "bfd_mach_mips10000" int32_t
  let bfd_mach_mips12000 = constant "bfd_mach_mips12000" int32_t
  let bfd_mach_mips14000 = constant "bfd_mach_mips14000" int32_t
  let bfd_mach_mips16000 = constant "bfd_mach_mips16000" int32_t
  let bfd_mach_mips16 = constant "bfd_mach_mips16" int32_t
  let bfd_mach_mips5 = constant "bfd_mach_mips5" int32_t
  let bfd_mach_mips_loongson_2e = constant "bfd_mach_mips_loongson_2e" int32_t
  let bfd_mach_mips_loongson_2f = constant "bfd_mach_mips_loongson_2f" int32_t
  let bfd_mach_mips_gs464 = constant "bfd_mach_mips_gs464" int32_t
  let bfd_mach_mips_gs464e = constant "bfd_mach_mips_gs464e" int32_t
  let bfd_mach_mips_gs264e = constant "bfd_mach_mips_gs264e" int32_t
  let bfd_mach_mips_octeon = constant "bfd_mach_mips_octeon" int32_t
  let bfd_mach_mips_octeonp = constant "bfd_mach_mips_octeonp" int32_t
  let bfd_mach_mips_octeon2 = constant "bfd_mach_mips_octeon2" int32_t
  let bfd_mach_mips_octeon3 = constant "bfd_mach_mips_octeon3" int32_t
  let bfd_mach_mipsisa32 = constant "bfd_mach_mipsisa32" int32_t
  let bfd_mach_mipsisa32r2 = constant "bfd_mach_mipsisa32r2" int32_t
  let bfd_mach_mipsisa32r3 = constant "bfd_mach_mipsisa32r3" int32_t
  let bfd_mach_mipsisa32r5 = constant "bfd_mach_mipsisa32r5" int32_t
  let bfd_mach_mipsisa32r6 = constant "bfd_mach_mipsisa32r6" int32_t
  let bfd_mach_mipsisa64 = constant "bfd_mach_mipsisa64" int32_t
  let bfd_mach_mipsisa64r2 = constant "bfd_mach_mipsisa64r2" int32_t
  let bfd_mach_mipsisa64r3 = constant "bfd_mach_mipsisa64r3" int32_t
  let bfd_mach_mipsisa64r5 = constant "bfd_mach_mipsisa64r5" int32_t
  let bfd_mach_mipsisa64r6 = constant "bfd_mach_mipsisa64r6" int32_t
  let bfd_mach_mips_micromips = constant "bfd_mach_mips_micromips" int32_t
  let bfd_mach_h8300 = constant "bfd_mach_h8300" int32_t
  let bfd_mach_h8300h = constant "bfd_mach_h8300h" int32_t
  let bfd_mach_h8300s = constant "bfd_mach_h8300s" int32_t
  let bfd_mach_h8300hn = constant "bfd_mach_h8300hn" int32_t
  let bfd_mach_h8300sn = constant "bfd_mach_h8300sn" int32_t
  let bfd_mach_h8300sx = constant "bfd_mach_h8300sx" int32_t
  let bfd_mach_h8300sxn = constant "bfd_mach_h8300sxn" int32_t
  let bfd_mach_ppc = constant "bfd_mach_ppc" int32_t
  let bfd_mach_ppc64 = constant "bfd_mach_ppc64" int32_t
  let bfd_mach_ppc_403 = constant "bfd_mach_ppc_403" int32_t
  let bfd_mach_ppc_403gc = constant "bfd_mach_ppc_403gc" int32_t
  let bfd_mach_ppc_405 = constant "bfd_mach_ppc_405" int32_t
  let bfd_mach_ppc_505 = constant "bfd_mach_ppc_505" int32_t
  let bfd_mach_ppc_601 = constant "bfd_mach_ppc_601" int32_t
  let bfd_mach_ppc_602 = constant "bfd_mach_ppc_602" int32_t
  let bfd_mach_ppc_603 = constant "bfd_mach_ppc_603" int32_t
  let bfd_mach_ppc_ec603e = constant "bfd_mach_ppc_ec603e" int32_t
  let bfd_mach_ppc_604 = constant "bfd_mach_ppc_604" int32_t
  let bfd_mach_ppc_620 = constant "bfd_mach_ppc_620" int32_t
  let bfd_mach_ppc_630 = constant "bfd_mach_ppc_630" int32_t
  let bfd_mach_ppc_750 = constant "bfd_mach_ppc_750" int32_t
  let bfd_mach_ppc_860 = constant "bfd_mach_ppc_860" int32_t
  let bfd_mach_ppc_a35 = constant "bfd_mach_ppc_a35" int32_t
  let bfd_mach_ppc_rs64ii = constant "bfd_mach_ppc_rs64ii" int32_t
  let bfd_mach_ppc_rs64iii = constant "bfd_mach_ppc_rs64iii" int32_t
  let bfd_mach_ppc_7400 = constant "bfd_mach_ppc_7400" int32_t
  let bfd_mach_ppc_e500 = constant "bfd_mach_ppc_e500" int32_t
  let bfd_mach_ppc_e500mc = constant "bfd_mach_ppc_e500mc" int32_t
  let bfd_mach_ppc_e500mc64 = constant "bfd_mach_ppc_e500mc64" int32_t
  let bfd_mach_ppc_e5500 = constant "bfd_mach_ppc_e5500" int32_t
  let bfd_mach_ppc_e6500 = constant "bfd_mach_ppc_e6500" int32_t
  let bfd_mach_ppc_titan = constant "bfd_mach_ppc_titan" int32_t
  let bfd_mach_ppc_vle = constant "bfd_mach_ppc_vle" int32_t
  let bfd_mach_rs6k = constant "bfd_mach_rs6k" int32_t
  let bfd_mach_rs6k_rs1 = constant "bfd_mach_rs6k_rs1" int32_t
  let bfd_mach_rs6k_rsc = constant "bfd_mach_rs6k_rsc" int32_t
  let bfd_mach_rs6k_rs2 = constant "bfd_mach_rs6k_rs2" int32_t
  let bfd_mach_hppa10 = constant "bfd_mach_hppa10" int32_t
  let bfd_mach_hppa11 = constant "bfd_mach_hppa11" int32_t
  let bfd_mach_hppa20 = constant "bfd_mach_hppa20" int32_t
  let bfd_mach_hppa20w = constant "bfd_mach_hppa20w" int32_t
  let bfd_mach_d10v = constant "bfd_mach_d10v" int32_t
  let bfd_mach_d10v_ts2 = constant "bfd_mach_d10v_ts2" int32_t
  let bfd_mach_d10v_ts3 = constant "bfd_mach_d10v_ts3" int32_t
  let bfd_mach_m6812_default = constant "bfd_mach_m6812_default" int32_t
  let bfd_mach_m6812 = constant "bfd_mach_m6812" int32_t
  let bfd_mach_m6812s = constant "bfd_mach_m6812s" int32_t
  let bfd_mach_s12z_default = constant "bfd_mach_s12z_default" int32_t
  let bfd_mach_z8001 = constant "bfd_mach_z8001" int32_t
  let bfd_mach_z8002 = constant "bfd_mach_z8002" int32_t
  let bfd_mach_sh = constant "bfd_mach_sh" int32_t
  let bfd_mach_arm_unknown = constant "bfd_mach_arm_unknown" int32_t
  let bfd_mach_arm_2 = constant "bfd_mach_arm_2" int32_t
  let bfd_mach_arm_2a = constant "bfd_mach_arm_2a" int32_t
  let bfd_mach_arm_3 = constant "bfd_mach_arm_3" int32_t
  let bfd_mach_arm_3M = constant "bfd_mach_arm_3M" int32_t
  let bfd_mach_arm_4 = constant "bfd_mach_arm_4" int32_t
  let bfd_mach_arm_4T = constant "bfd_mach_arm_4T" int32_t
  let bfd_mach_arm_5 = constant "bfd_mach_arm_5" int32_t
  let bfd_mach_arm_5T = constant "bfd_mach_arm_5T" int32_t
  let bfd_mach_arm_5TE = constant "bfd_mach_arm_5TE" int32_t
  let bfd_mach_arm_XScale = constant "bfd_mach_arm_XScale" int32_t
  let bfd_mach_arm_ep9312 = constant "bfd_mach_arm_ep9312" int32_t
  let bfd_mach_arm_iWMMXt = constant "bfd_mach_arm_iWMMXt" int32_t
  let bfd_mach_arm_iWMMXt2 = constant "bfd_mach_arm_iWMMXt2" int32_t
  let bfd_mach_arm_5TEJ = constant "bfd_mach_arm_5TEJ" int32_t
  let bfd_mach_arm_6 = constant "bfd_mach_arm_6" int32_t
  let bfd_mach_arm_6KZ = constant "bfd_mach_arm_6KZ" int32_t
  let bfd_mach_arm_6T2 = constant "bfd_mach_arm_6T2" int32_t
  let bfd_mach_arm_6K = constant "bfd_mach_arm_6K" int32_t
  let bfd_mach_arm_7 = constant "bfd_mach_arm_7" int32_t
  let bfd_mach_arm_6M = constant "bfd_mach_arm_6M" int32_t
  let bfd_mach_arm_6SM = constant "bfd_mach_arm_6SM" int32_t
  let bfd_mach_arm_7EM = constant "bfd_mach_arm_7EM" int32_t
  let bfd_mach_arm_8 = constant "bfd_mach_arm_8" int32_t
  let bfd_mach_arm_8R = constant "bfd_mach_arm_8R" int32_t
  let bfd_mach_arm_8M_BASE = constant "bfd_mach_arm_8M_BASE" int32_t
  let bfd_mach_arm_8M_MAIN = constant "bfd_mach_arm_8M_MAIN" int32_t
  let bfd_mach_arm_8_1M_MAIN = constant "bfd_mach_arm_8_1M_MAIN" int32_t
  let bfd_mach_arm_9 = constant "bfd_mach_arm_9" int32_t
  let bfd_mach_n1 = constant "bfd_mach_n1" int32_t
  let bfd_mach_n1h = constant "bfd_mach_n1h" int32_t
  let bfd_mach_n1h_v2 = constant "bfd_mach_n1h_v2" int32_t
  let bfd_mach_n1h_v3 = constant "bfd_mach_n1h_v3" int32_t
  let bfd_mach_n1h_v3m = constant "bfd_mach_n1h_v3m" int32_t
  let bfd_mach_tic3x = constant "bfd_mach_tic3x" int32_t
  let bfd_mach_tic4x = constant "bfd_mach_tic4x" int32_t
  let bfd_mach_v850 = constant "bfd_mach_v850" int32_t
  let bfd_mach_arc_a4 = constant "bfd_mach_arc_a4" int32_t
  let bfd_mach_arc_a5 = constant "bfd_mach_arc_a5" int32_t
  let bfd_mach_arc_arc600 = constant "bfd_mach_arc_arc600" int32_t
  let bfd_mach_arc_arc601 = constant "bfd_mach_arc_arc601" int32_t
  let bfd_mach_arc_arc700 = constant "bfd_mach_arc_arc700" int32_t
  let bfd_mach_arc_arcv2 = constant "bfd_mach_arc_arcv2" int32_t
  let bfd_mach_mn10300 = constant "bfd_mach_mn10300" int32_t
  let bfd_mach_am33 = constant "bfd_mach_am33" int32_t
  let bfd_mach_am33_2 = constant "bfd_mach_am33_2" int32_t
  let bfd_mach_frv = constant "bfd_mach_frv" int32_t
  let bfd_mach_frvsimple = constant "bfd_mach_frvsimple" int32_t
  let bfd_mach_fr300 = constant "bfd_mach_fr300" int32_t
  let bfd_mach_fr400 = constant "bfd_mach_fr400" int32_t
  let bfd_mach_fr450 = constant "bfd_mach_fr450" int32_t
  let bfd_mach_fr500 = constant "bfd_mach_fr500" int32_t
  let bfd_mach_fr550 = constant "bfd_mach_fr550" int32_t
  let bfd_mach_moxie = constant "bfd_mach_moxie" int32_t
  let bfd_mach_ft32 = constant "bfd_mach_ft32" int32_t
  let bfd_mach_ft32b = constant "bfd_mach_ft32b" int32_t
  let bfd_mach_mep = constant "bfd_mach_mep" int32_t
  let bfd_mach_metag = constant "bfd_mach_metag" int32_t
  let bfd_mach_ia64_elf64 = constant "bfd_mach_ia64_elf64" int32_t
  let bfd_mach_ia64_elf32 = constant "bfd_mach_ia64_elf32" int32_t
  let bfd_mach_ip2022 = constant "bfd_mach_ip2022" int32_t
  let bfd_mach_ip2022ext = constant "bfd_mach_ip2022ext" int32_t
  let bfd_mach_iq2000 = constant "bfd_mach_iq2000" int32_t
  let bfd_mach_iq10 = constant "bfd_mach_iq10" int32_t
  let bfd_mach_bpf = constant "bfd_mach_bpf" int32_t
  let bfd_mach_xbpf = constant "bfd_mach_xbpf" int32_t
  let bfd_mach_epiphany16 = constant "bfd_mach_epiphany16" int32_t
  let bfd_mach_epiphany32 = constant "bfd_mach_epiphany32" int32_t
  let bfd_mach_ms1 = constant "bfd_mach_ms1" int32_t
  let bfd_mach_mrisc2 = constant "bfd_mach_mrisc2" int32_t
  let bfd_mach_ms2 = constant "bfd_mach_ms2" int32_t
  let bfd_mach_avr1 = constant "bfd_mach_avr1" int32_t
  let bfd_mach_avr2 = constant "bfd_mach_avr2" int32_t
  let bfd_mach_avr25 = constant "bfd_mach_avr25" int32_t
  let bfd_mach_avr3 = constant "bfd_mach_avr3" int32_t
  let bfd_mach_avr31 = constant "bfd_mach_avr31" int32_t
  let bfd_mach_avr35 = constant "bfd_mach_avr35" int32_t
  let bfd_mach_avr4 = constant "bfd_mach_avr4" int32_t
  let bfd_mach_avr5 = constant "bfd_mach_avr5" int32_t
  let bfd_mach_avr51 = constant "bfd_mach_avr51" int32_t
  let bfd_mach_avr6 = constant "bfd_mach_avr6" int32_t
  let bfd_mach_avrtiny = constant "bfd_mach_avrtiny" int32_t
  let bfd_mach_avrxmega1 = constant "bfd_mach_avrxmega1" int32_t
  let bfd_mach_avrxmega2 = constant "bfd_mach_avrxmega2" int32_t
  let bfd_mach_avrxmega3 = constant "bfd_mach_avrxmega3" int32_t
  let bfd_mach_avrxmega4 = constant "bfd_mach_avrxmega4" int32_t
  let bfd_mach_avrxmega5 = constant "bfd_mach_avrxmega5" int32_t
  let bfd_mach_avrxmega6 = constant "bfd_mach_avrxmega6" int32_t
  let bfd_mach_avrxmega7 = constant "bfd_mach_avrxmega7" int32_t
  let bfd_mach_bfin = constant "bfd_mach_bfin" int32_t
  let bfd_mach_cr16 = constant "bfd_mach_cr16" int32_t
  let bfd_mach_crx = constant "bfd_mach_crx" int32_t
  let bfd_mach_cris_v0_v10 = constant "bfd_mach_cris_v0_v10" int32_t
  let bfd_mach_cris_v32 = constant "bfd_mach_cris_v32" int32_t
  let bfd_mach_cris_v10_v32 = constant "bfd_mach_cris_v10_v32" int32_t
  let bfd_mach_riscv32 = constant "bfd_mach_riscv32" int32_t
  let bfd_mach_riscv64 = constant "bfd_mach_riscv64" int32_t
  let bfd_mach_s390_31 = constant "bfd_mach_s390_31" int32_t
  let bfd_mach_s390_64 = constant "bfd_mach_s390_64" int32_t
  let bfd_mach_score3 = constant "bfd_mach_score3" int32_t
  let bfd_mach_score7 = constant "bfd_mach_score7" int32_t
  let bfd_mach_xstormy16 = constant "bfd_mach_xstormy16" int32_t
  let bfd_mach_msp11 = constant "bfd_mach_msp11" int32_t
  let bfd_mach_msp110 = constant "bfd_mach_msp110" int32_t
  let bfd_mach_msp12 = constant "bfd_mach_msp12" int32_t
  let bfd_mach_msp13 = constant "bfd_mach_msp13" int32_t
  let bfd_mach_msp14 = constant "bfd_mach_msp14" int32_t
  let bfd_mach_msp15 = constant "bfd_mach_msp15" int32_t
  let bfd_mach_msp16 = constant "bfd_mach_msp16" int32_t
  let bfd_mach_msp20 = constant "bfd_mach_msp20" int32_t
  let bfd_mach_msp21 = constant "bfd_mach_msp21" int32_t
  let bfd_mach_msp22 = constant "bfd_mach_msp22" int32_t
  let bfd_mach_msp23 = constant "bfd_mach_msp23" int32_t
  let bfd_mach_msp24 = constant "bfd_mach_msp24" int32_t
  let bfd_mach_msp26 = constant "bfd_mach_msp26" int32_t
  let bfd_mach_msp31 = constant "bfd_mach_msp31" int32_t
  let bfd_mach_msp32 = constant "bfd_mach_msp32" int32_t
  let bfd_mach_msp33 = constant "bfd_mach_msp33" int32_t
  let bfd_mach_msp41 = constant "bfd_mach_msp41" int32_t
  let bfd_mach_msp42 = constant "bfd_mach_msp42" int32_t
  let bfd_mach_msp43 = constant "bfd_mach_msp43" int32_t
  let bfd_mach_msp44 = constant "bfd_mach_msp44" int32_t
  let bfd_mach_msp430x = constant "bfd_mach_msp430x" int32_t
  let bfd_mach_msp46 = constant "bfd_mach_msp46" int32_t
  let bfd_mach_msp47 = constant "bfd_mach_msp47" int32_t
  let bfd_mach_msp54 = constant "bfd_mach_msp54" int32_t
  let bfd_mach_xgate = constant "bfd_mach_xgate" int32_t
  let bfd_mach_xtensa = constant "bfd_mach_xtensa" int32_t
  let bfd_mach_z80strict = constant "bfd_mach_z80strict" int32_t
  let bfd_mach_z180 = constant "bfd_mach_z180" int32_t
  let bfd_mach_z80 = constant "bfd_mach_z80" int32_t
  let bfd_mach_ez80_z80 = constant "bfd_mach_ez80_z80" int32_t
  let bfd_mach_ez80_adl = constant "bfd_mach_ez80_adl" int32_t
  let bfd_mach_z80n = constant "bfd_mach_z80n" int32_t
  let bfd_mach_z80full = constant "bfd_mach_z80full" int32_t
  let bfd_mach_gbz80 = constant "bfd_mach_gbz80" int32_t
  let bfd_mach_r800 = constant "bfd_mach_r800" int32_t
  let bfd_mach_lm32 = constant "bfd_mach_lm32" int32_t
  let bfd_mach_kv3_unknown = constant "bfd_mach_kv3_unknown" int32_t
  let bfd_mach_kv3_1 = constant "bfd_mach_kv3_1" int32_t
  let bfd_mach_kv3_1_64 = constant "bfd_mach_kv3_1_64" int32_t
  let bfd_mach_kv3_1_usr = constant "bfd_mach_kv3_1_usr" int32_t
  let bfd_mach_kv3_2 = constant "bfd_mach_kv3_2" int32_t
  let bfd_mach_kv3_2_64 = constant "bfd_mach_kv3_2_64" int32_t
  let bfd_mach_kv3_2_usr = constant "bfd_mach_kv3_2_usr" int32_t
  let bfd_mach_kv4_1 = constant "bfd_mach_kv4_1" int32_t
  let bfd_mach_kv4_1_64 = constant "bfd_mach_kv4_1_64" int32_t
  let bfd_mach_kv4_1_usr = constant "bfd_mach_kv4_1_usr" int32_t
  let bfd_mach_tilepro = constant "bfd_mach_tilepro" int32_t
  let bfd_mach_tilegx = constant "bfd_mach_tilegx" int32_t
  let bfd_mach_tilegx32 = constant "bfd_mach_tilegx32" int32_t
  let bfd_mach_aarch64 = constant "bfd_mach_aarch64" int32_t
  let bfd_mach_aarch64_8R = constant "bfd_mach_aarch64_8R" int32_t
  let bfd_mach_aarch64_ilp32 = constant "bfd_mach_aarch64_ilp32" int32_t
  let bfd_mach_aarch64_llp64 = constant "bfd_mach_aarch64_llp64" int32_t
  let bfd_mach_visium = constant "bfd_mach_visium" int32_t
  let bfd_mach_wasm32 = constant "bfd_mach_wasm32" int32_t
  let bfd_mach_pru = constant "bfd_mach_pru" int32_t
  let bfd_mach_ck_unknown = constant "bfd_mach_ck_unknown" int32_t
  let bfd_mach_ck510 = constant "bfd_mach_ck510" int32_t
  let bfd_mach_ck610 = constant "bfd_mach_ck610" int32_t
  let bfd_mach_ck801 = constant "bfd_mach_ck801" int32_t
  let bfd_mach_ck802 = constant "bfd_mach_ck802" int32_t
  let bfd_mach_ck803 = constant "bfd_mach_ck803" int32_t
  let bfd_mach_ck807 = constant "bfd_mach_ck807" int32_t
  let bfd_mach_ck810 = constant "bfd_mach_ck810" int32_t
  let bfd_mach_ck860 = constant "bfd_mach_ck860" int32_t
  let bfd_mach_loongarch32 = constant "bfd_mach_loongarch32" int32_t
  let bfd_mach_loongarch64 = constant "bfd_mach_loongarch64" int32_t
end
