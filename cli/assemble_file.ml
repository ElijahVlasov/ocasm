open Rv32.Parser_frontend
open Rv32.Instruction
open Elf.Elf_header
open Elf.Program_header

let elf_header_size = 52
let program_header_size = 32

let mk_elf_header instructions_len =
  let ( + ) = Int32.add in
  let ( * ) = Int32.mul in
  {
    e_ident =
      Elf_ident.mk Elf_class.elf_class_32 Elf_data.elf_data_2lsb
        Elf_version.elf_version_current;
    e_type = Elf_type.elf_type_exec;
    e_machine = Elf_machine.elf_machine_riscv;
    e_version = 1l;
    e_entry = 0l;
    e_phoff = 64l;
    e_shoff = 64l + (4l * Int32.of_int instructions_len);
    e_flags = 0l;
    e_ehsize = elf_header_size;
    e_phentsize = program_header_size;
    e_phnum = 1;
    e_shentsize = 40;
    e_shnum = 1;
    e_shstrndx = 1;
  }

let mk_program_header_table instructions_len =
  let ( |: ) = Program_flags.( |: ) in
  let ( * ) = Int32.mul in
  let code_segment =
    {
      p_type = Program_type.pt_load;
      p_offset = 0l;
      p_vaddr = 0x400000l;
      p_paddr = 0x400000l;
      p_filesz = Int32.of_int instructions_len * 4l;
      p_memsz = Int32.of_int instructions_len * 4l;
      p_flags = Program_flags.pf_x |: Program_flags.pf_r;
      p_align = 4096l;
    }
  in
  [ code_segment ]

let emit_elf instructions =
  (* TODO: change this *)
  let buf = Bytes.create 1024 in
  let instructions_len = List.length instructions in
  let elf_header = mk_elf_header instructions_len in
  let elf_program_header_table = mk_program_header_table instructions_len in
  Elf.Elf_header.write_into elf_header buf 0;
  let offset = ref elf_header_size in
  List.iter
    (fun elf_program_header ->
      Elf.Program_header.write_into elf_program_header buf !offset;
      offset := !offset + program_header_size)
    elf_program_header_table;
  List.iter
    (fun instruction ->
      let instruction = RawInstruction.to_int instruction in
      Bytes.set_int32_le buf !offset instruction;
      offset := !offset + 4)
    instructions;
  buf

let assemble program =
  let structured_instructions = parse_program program in
  match structured_instructions with
  | Ok structured_instructions ->
      let raw_instructions =
        List.map
          (fun structured_instruction -> ast_to_raw structured_instruction)
          structured_instructions
      in
      let elf_file = emit_elf raw_instructions in
      let oc = open_out_bin "a.out" in
      output_bytes oc elf_file;
      close_out oc
  | Error error -> failwith "Failed to parse instruction"

let assemble_file filename =
  let ic = open_in filename in
  let program = really_input_string ic (in_channel_length ic) in
  let () = close_in ic in
  assemble program
