open Base
module Section = Section

type 'a symbol = 'a Symbol.symbol

open Bfd.BfdMonad

let ( let* ) = ( >>= )

let write_object_file ~word_type ~name ~sections ~symtab =
  let target = "elf32-bigriscv" in
  Bfd.with_bfd ~file_name:name ~target
  @@
  let* _ = Bfd.set_object_format in
  let* sections = Section_writer.init_sections word_type sections in
  let* _ = Symbol.set_symtab word_type sections symtab in
  Section_writer.write_section_contents word_type sections
