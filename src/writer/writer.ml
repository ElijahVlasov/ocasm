open Base
open Ocasm_bfd
open Ocasm_bfd.Bfd.BfdMonad

let ( let* ) = ( >>= )

let write_object_file ~wt ~file_name ~sections ~symtab =
  let target = "elf32-littleriscv" in
  Bfd.with_bfd ~file_name ~target
  @@
  let* _ =
    Bfd.set_arch_mach Ocasm_bfd.Arch.Riscv Ocasm_bfd.Arch.Machine.riscv32
  in
  let* _ = Bfd.set_object_format in
  let* sections = Section_writer.init_sections wt sections in
  let* _ = Symtab_writer.set_symtab wt sections symtab in
  Section_writer.write_section_contents wt sections
