module Program_type = struct
  type t = int32

  let to_int32 x = x
  let pt_null = 0l
  let pt_load = 1l
  let pt_dynamic = 2l
  let pt_interp = 3l
  let pt_note = 4l
  let pt_shlib = 5l
  let pt_phdr = 6l
  let pt_loproc = 0x70000000l
  let pt_hiproc = 0x7fffffffl
end

type program_type = Program_type.t

module Program_flags = struct
  type t = int32

  let ( |: ) = Int32.logor
  let to_int32 x = x
  let pf_x = 0x1l (* Execute *)
  let pf_w = 0x2l (* Write *)
  let pf_r = 0x4l (* Read *)
  let pf_maskproc = 0xf0000000l
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

let write_into (p : elf32_phdr) (buf : bytes) (pos : int) =
  Bytes.set_int32_le buf (pos + 0) (Program_type.to_int32 p.p_type);
  Bytes.set_int32_le buf (pos + 4) p.p_offset;
  Bytes.set_int32_le buf (pos + 8) p.p_vaddr;
  Bytes.set_int32_le buf (pos + 12) p.p_paddr;
  Bytes.set_int32_le buf (pos + 16) p.p_filesz;
  Bytes.set_int32_le buf (pos + 20) p.p_memsz;
  Bytes.set_int32_le buf (pos + 24) (Program_flags.to_int32 p.p_flags);
  Bytes.set_int32_le buf (pos + 28) p.p_align;
  ()

let to_bytes (p : elf32_phdr) =
  let b = Bytes.create 32 in
  write_into p b 0;
  b
