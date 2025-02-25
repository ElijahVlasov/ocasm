module Section_type = struct
  type t = int32

  let to_int32 x = x
  let sht_null = 0l
  let sht_progbits = 1l
  let sht_symtab = 2l
  let sht_strtab = 3l
  let sht_rela = 4l
  let sht_hash = 5l
  let sht_dynamic = 6l
  let sht_note = 7l
  let sht_nobits = 8l
  let sht_rel = 9l
  let sht_shlib = 10l
  let sht_dynsym = 11l
  let sht_loproc = 0x70000000l
  let sht_hiproc = 0x7fffffffl
  let sht_louser = 0x80000000l
  let sht_hiuser = 0xffffffffl
end

type section_type = Section_type.t

module Section_flags = struct
  type t = int32

  let to_int32 x = x
  let shf_write = 0x1l
  let shf_alloc = 0x2l
  let shf_execinstr = 0x4l
  let shf_maskproc = 0xf0000000l
end

type section_flags = Section_flags.t

type elf32_shdr = {
  sh_name : int32; (* Section name (string tbl index) *)
  sh_type : section_type; (* Section type *)
  sh_flags : section_flags; (* Section flags *)
  sh_addr : int32; (* Section virtual addr at execution *)
  sh_offset : int32; (* Section file offset *)
  sh_size : int32; (* Section size in bytes *)
  sh_link : int32; (* Link to another section *)
  sh_info : int32; (* Additional section information *)
  sh_addralign : int32; (* Section alignment *)
  sh_entsize : int32; (* Entry size if section holds table *)
}

let write_into (s : elf32_shdr) (buf : bytes) (pos : int) =
  Bytes.set_int32_le buf (pos + 0) s.sh_name;
  Bytes.set_int32_le buf (pos + 4) (Section_type.to_int32 s.sh_type);
  Bytes.set_int32_le buf (pos + 8) (Section_flags.to_int32 s.sh_flags);
  Bytes.set_int32_le buf (pos + 12) s.sh_addr;
  Bytes.set_int32_le buf (pos + 16) s.sh_offset;
  Bytes.set_int32_le buf (pos + 20) s.sh_size;
  Bytes.set_int32_le buf (pos + 24) s.sh_link;
  Bytes.set_int32_le buf (pos + 28) s.sh_info;
  Bytes.set_int32_le buf (pos + 32) s.sh_addralign;
  Bytes.set_int32_le buf (pos + 36) s.sh_entsize;
  ()

let to_bytes (s : elf32_shdr) =
  let b = Bytes.create 40 in
  write_into s b 0;
  b
