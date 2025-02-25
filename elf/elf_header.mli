module Elf_class : sig
  type t

  val to_char : t -> char
  val elf_class_none : t
  val elf_class_32 : t
  val elf_class_64 : t
end

type elf_class = Elf_class.t

module Elf_data : sig
  type t

  val to_char : t -> char
  val elf_data_none : t
  val elf_data_2lsb : t
  val elf_data_2msb : t
end

type elf_data = Elf_data.t

module Elf_version : sig
  type t

  val to_char : t -> char
  val elf_version_none : t
  val elf_version_current : t
end

type elf_version = Elf_version.t

module Elf_ident : sig
  type t

  val mk : elf_class -> elf_data -> elf_version -> t
  val to_bytes : t -> bytes
end

type elf_ident = Elf_ident.t

module Elf_type : sig
  type t

  val to_int : t -> int
  val elf_type_none : t
  val elf_type_rel : t
  val elf_type_exec : t
  val elf_type_dyn : t
  val elf_type_core : t
  val elf_type_loproc : t
  val elf_type_hiproc : t
end

type elf_type = Elf_type.t

module Elf_machine : sig
  type t

  val to_int : t -> int
  val elf_machine_none : t
  val elf_machine_sparc : t
  val elf_machine_x86 : t
  val elf_machine_m68k : t
  val elf_machine_m88k : t
  val elf_machine_i386 : t
  val elf_machine_mips : t
  val elf_machine_m68000 : t
  val elf_machine_rs6000 : t
  val elf_machine_m88000 : t
  val elf_machine_i960 : t
  val elf_machine_riscv : t
end

type elf_machine = Elf_machine.t

type elf32_ehdr = {
  e_ident : elf_ident; (* size = 16 *)
  e_type : elf_type; (* 16-bit *)
  e_machine : elf_machine; (* 16-bit *)
  e_version : int32; (* 32-bit *)
  e_entry : int32; (* 32-bit *)
  e_phoff : int32; (* 32-bit *)
  e_shoff : int32; (* 32-bit *)
  e_flags : int32; (* 32-bit *)
  e_ehsize : int; (* 16-bit *)
  e_phentsize : int; (* 16-bit *)
  e_phnum : int; (* 16-bit *)
  e_shentsize : int; (* 16-bit *)
  e_shnum : int; (* 16-bit *)
  e_shstrndx : int; (* 16-bit *)
}

val to_bytes : elf32_ehdr -> bytes
val write_into : elf32_ehdr -> bytes -> int -> unit
