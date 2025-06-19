open Base
open Bfd
open Bfd.CArray
open Ocasm_assembler

let ( let* ) = BfdMonad.( >>= )

let set_symtab word_type sections symtab =
  let open BfdMonad in
  let open Symbol in
  let open Section_writer in
  let* symtab =
    all
    @@
    let mk_symbol { section; name; value; flags } =
      let sec = (Hashtbl.find_exn sections section).bfd_sec in
      Bfd.make_symbol ~sec ~name ~flags
        ~value:(WordType.word_to_int64 word_type value)
    in
    List.map ~f:mk_symbol symtab
  in
  Bfd.set_symtab symtab
