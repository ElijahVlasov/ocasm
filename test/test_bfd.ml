open Bfd.BfdMonad

let ( let* ) x f = bind x ~f

let () =
  let ( * ) = Int64.mul in
  try
    Bfd.with_bfd "lol" "elf32-bigriscv"
    @@
    let* _ = Bfd.set_object_format in
    let* sec = Bfd.make_section ".text" in
    ignore @@ Bfd.set_section_size sec (3L * 4L);
    let contents = [ 0x41l; 0x02l; 0x03l ] in
    let* sym = Bfd.make_symbol "dummy" sec Bfd.Symbol_flags.bsf_global 0x123L in
    Bfd.set_section_flags sec Bfd.Section_flags.sec_has_contents;
    let symtable = [ sym ] in
    let* _x = Bfd.set_symtab symtable in
    Bfd.set_section_contents Bfd.Word32 sec contents 0x00L
  with Bfd.BfdException err -> print_string (Bfd.Error.to_string err)
