module Program_type : sig
  type t

  val to_int32 : t -> int32
  val pt_null : t
  val pt_load : t
  val pt_dynamic : t
  val pt_interp : t
  val pt_note : t
  val pt_shlib : t
  val pt_phdr : t
  val pt_loproc : t
  val pt_hiproc : t
end

type program_type = Program_type.t

module Program_flags : sig
  type t

  val to_int32 : t -> int32
  val ( |: ) : t -> t -> t
  val pf_x : t
  val pf_w : t
  val pf_r : t
  val pf_maskproc : t
end

type program_flags = Program_flags.t

type elf32_phdr = {
  p_type : program_type; (* Segment type *)
  p_offset : int32; (* Segment file offset *)
  p_vaddr : int32; (* Segment virtual address *)
  p_paddr : int32; (* Segment physical address *)
  p_filesz : int32; (* Segment size in file *)
  p_memsz : int32; (* Segment size in memory *)
  p_flags : program_flags; (* Segment flags *)
  p_align : int32; (* Segment alignment *)
}

val write_into : elf32_phdr -> bytes -> int -> unit
val to_bytes : elf32_phdr -> bytes
