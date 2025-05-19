open Base
open Bfd
open Bfd.CArray
open Section_writer

type 'a symbol = {
  section : Section.t;
  name : string;
  value : 'a;
  flags : Bfd.Symbol_flags.t;
}

let ( let* ) = BfdMonad.( >>= )

let set_symtab word_type sections symtab =
  let open BfdMonad in
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
