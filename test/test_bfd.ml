open! Import

let test_bfd () =
  let open Ocasm_bfd.Bfd in
  let open Ocasm_bfd.Bfd.BfdMonad in
  let ( let* ) x f = bind x ~f in
  let ( * ) = Int64.mul in
  (try
     with_bfd ~file_name:"lol" ~target:"elf32-bigriscv"
     @@
     let* _ = set_object_format in
     let* sec = make_section ".text" in
     ignore @@ set_section_size sec (3L * 4L);
     let content = [ 0x41l; 0x02l; 0x03l ] in
     let* sym =
       make_symbol ~name:"dummy" ~sec ~flags:Symbol_flags.bsf_global
         ~value:0x123L
     in
     set_section_flags sec Section_flags.sec_has_contents;
     let symtable = [ sym ] in
     let* _x = set_symtab symtable in
     set_section_contents Word32 ~sec ~content ~file_offset:0x00L
   with BfdException err -> Stdlib.print_string (Error.to_string err));
  Alcotest.check int "" 0 0

let suite = [ test_case "First" `Quick test_bfd ]
