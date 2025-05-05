open Bfd.BfdMonad

let ( let* ) x f = bind x ~f

let () =
  try
    Bfd.with_bfd "lol" "elf32-littleriscv"
    @@
    let* _ = Bfd.set_object_format in
    let* sec = Bfd.make_section ".text" in
    let contents = [ 0x01l; 0x02l; 0x03l ] in
    Bfd.set_section_flags sec Bfd.Section_flags.sec_has_contents;
    let* _x = Bfd.set_section_contents Bfd.Word32 sec contents 0x00L in
    return ()
  with Bfd.BfdException err -> print_string (Bfd.Error.to_string err)
