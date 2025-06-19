open Base
open Bfd.BfdMonad

let ( let* ) = ( >>= )

let write_object_file ~word_type ~file_name ~sections ~symtab =
  let target = "elf32-littleriscv" in
  Bfd.with_bfd ~file_name ~target
  @@
  let* _ = Bfd.set_arch_mach Bfd.Arch.Riscv Bfd.Arch.Machine.riscv32 in
  let* _ = Bfd.set_object_format in
  let* sections = Section_writer.init_sections word_type sections in
  let* _ = Symtab_writer.set_symtab word_type sections symtab in
  Section_writer.write_section_contents word_type sections
