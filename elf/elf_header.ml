module Elf_class = struct
  type t = char

  let to_char x = x
  let elf_class_none = '\x00'
  let elf_class_32 = '\x01'
  let elf_class_64 = '\x02'
end

type elf_class = Elf_class.t

module Elf_data = struct
  type t = char

  let to_char x = x
  let elf_data_none = '\x00'
  let elf_data_2lsb = '\x01'
  let elf_data_2msb = '\x02'
end

type elf_data = Elf_data.t

module Elf_version = struct
  type t = char

  let to_char x = x
  let elf_version_none = '\x00'
  let elf_version_current = '\x01'
end

type elf_version = Elf_version.t

module Elf_ident = struct
  type t = bytes

  let mk class_ data version =
    let b = Bytes.make 16 '\x00' in
    Bytes.set b 0 '\x7f';
    Bytes.set b 1 'E';
    Bytes.set b 2 'L';
    Bytes.set b 3 'F';
    Bytes.set b 4 (Elf_class.to_char class_);
    Bytes.set b 5 (Elf_data.to_char data);
    Bytes.set b 6 (Elf_version.to_char version);
    b

  let to_bytes x = x
end

type elf_ident = Elf_ident.t

module Elf_type = struct
  type t = int

  let to_int x = x
  let elf_type_none = 0
  let elf_type_rel = 1
  let elf_type_exec = 2
  let elf_type_dyn = 3
  let elf_type_core = 4
  let elf_type_loproc = 0xfe00
  let elf_type_hiproc = 0xffff
end

type elf_type = Elf_type.t

module Elf_machine = struct
  type t = int

  let to_int x = x
  let elf_machine_none = 0
  let elf_machine_sparc = 2
  let elf_machine_x86 = 3
  let elf_machine_m68k = 4
  let elf_machine_m88k = 5
  let elf_machine_i386 = 6
  let elf_machine_mips = 8
  let elf_machine_m68000 = 10
  let elf_machine_rs6000 = 11
  let elf_machine_m88000 = 12
  let elf_machine_i960 = 13
  let elf_machine_riscv = 0xF3
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

let write_into (e : elf32_ehdr) (buf : bytes) (pos : int) =
  Bytes.blit (Elf_ident.to_bytes e.e_ident) 0 buf pos 16;
  Bytes.set_int16_le buf (pos + 16) (Elf_type.to_int e.e_type);
  Bytes.set_int16_le buf (pos + 18) (Elf_machine.to_int e.e_machine);
  Bytes.set_int32_le buf (pos + 20) e.e_version;
  Bytes.set_int32_le buf (pos + 24) e.e_entry;
  Bytes.set_int32_le buf (pos + 28) e.e_phoff;
  Bytes.set_int32_le buf (pos + 32) e.e_shoff;
  Bytes.set_int32_le buf (pos + 36) e.e_flags;
  Bytes.set_int16_le buf (pos + 40) e.e_ehsize;
  Bytes.set_int16_le buf (pos + 42) e.e_phentsize;
  Bytes.set_int16_le buf (pos + 44) e.e_phnum;
  Bytes.set_int16_le buf (pos + 46) e.e_shentsize;
  Bytes.set_int16_le buf (pos + 48) e.e_shnum;
  Bytes.set_int16_le buf (pos + 50) e.e_shstrndx;
  ()

let to_bytes (e : elf32_ehdr) =
  let b = Bytes.create 52 in
  write_into e b 0;
  b
