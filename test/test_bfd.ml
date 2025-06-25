open Ocasm_bfd
open Ocasm_bfd.Bfd.BfdMonad

let ( let* ) x f = bind x ~f

let () =
  let ( * ) = Int64.mul in
  try
    Bfd.with_bfd ~file_name:"lol" ~target:"elf32-bigriscv"
    @@
    let* _ = Bfd.set_object_format in
    let* sec = Bfd.make_section ".text" in
    ignore @@ Bfd.set_section_size sec (3L * 4L);
    let content = [ 0x41l; 0x02l; 0x03l ] in
    let* sym =
      Bfd.make_symbol ~name:"dummy" ~sec ~flags:Bfd.Symbol_flags.bsf_global
        ~value:0x123L
    in
    Bfd.set_section_flags sec Bfd.Section_flags.sec_has_contents;
    let symtable = [ sym ] in
    let* _x = Bfd.set_symtab symtable in
    Bfd.set_section_contents Word32 ~sec ~content ~file_offset:0x00L
  with Bfd.BfdException err -> print_string (Bfd.Error.to_string err)
