open Base

type t = Data | Text | Bss [@@deriving eq, ord, hash, sexp]

let to_string = function Data -> ".data" | Text -> ".text" | Bss -> ".bss"

let section_flags (section : t) : Bfd.Section_flags.t =
  let open Bfd.Section_flags in
  match section with
  | Data -> sec_has_contents |+ sec_data
  | Text -> sec_has_contents |+ sec_code
  | Bss -> sec_alloc
