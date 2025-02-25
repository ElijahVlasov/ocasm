module Section_type : sig
  type t

  val to_int32 : t -> int32
  val sht_null : t
  val sht_progbits : t
  val sht_symtab : t
  val sht_strtab : t
  val sht_rela : t
  val sht_hash : t
  val sht_dynamic : t
  val sht_note : t
  val sht_nobits : t
  val sht_rel : t
  val sht_shlib : t
  val sht_dynsym : t
  val sht_loproc : t
  val sht_hiproc : t
  val sht_louser : t
  val sht_hiuser : t
end

type section_type = Section_type.t

module Section_flags : sig
  type t

  val to_int32 : t -> int32
  val shf_write : t
  val shf_alloc : t
  val shf_execinstr : t
  val shf_maskproc : t
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

val write_into : elf32_shdr -> bytes -> int -> unit
val to_bytes : elf32_shdr -> bytes
