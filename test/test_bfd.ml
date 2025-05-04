open Bfd.BfdMonad

let ( let* ) x f = bind x ~f
(* let ( let+ ) x f = map ~f x *)

let () =
  (* let ( * ) = Int64.mul in *)
  Bfd.with_bfd "lol" "elf32-littleriscv"
  @@
  let* is_success = Bfd.set_object_format in
  if not is_success then return @@ print_string "not good"
  else (
    print_string "good";
    let* sec = Bfd.make_section ".text" in
    (* ignore @@ Bfd.set_section_size sec (3L * 4L); *)
    let contents = [ 0x01l; 0x02l; 0x03l ] in
    let* is_success =
      ignore @@ Bfd.set_section_flags sec Bfd__Section_flags.sec_has_contents;
      Bfd.set_section_contents Bfd.Int32 sec contents 0x00L
    in
    if is_success then return @@ print_string "test"
    else return @@ print_string "not so much")
