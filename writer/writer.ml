open Base

module Section : sig
  type t = Data | Text | Bss

  include Equal.S with type t := t

  val to_string : t -> string
end = struct
  type t = Data | Text | Bss [@@deriving eq]

  let to_string = function Data -> ".data" | Text -> ".text" | Bss -> ".bss"
end

let target = "elf32-bigriscv"

type 'a section_info = { bfd_sec : Bfd.asection; contents : 'a list }

open Bfd.BfdMonad

let ( let* ) = ( >>= )

let section_flags (section : Section.t) : Bfd.Section_flags.t =
  let open Bfd.Section_flags in
  match section with
  | Section.Data -> sec_has_contents |+ sec_data
  | Section.Text -> sec_has_contents |+ sec_code
  | Section.Bss -> sec_alloc

let make_empty_section_and_set_flags (section : Section.t) : Bfd.asection t =
  let* asection = Bfd.make_section (Section.to_string section) in
  let flags = section_flags section in
  Bfd.set_section_flags asection flags;
  return asection

let make_empty_sections (sections : (Section.t * 'a list) list) :
    (Section.t * 'a section_info) list t =
  all
  @@ List.map sections ~f:(fun (section, contents) ->
         let* bfd_sec = make_empty_section_and_set_flags section in
         let sec_info = { bfd_sec; contents } in
         return (section, sec_info))

let write_object_file (name : string) (sections : (Section.t * 'a list) list) =
  Bfd.with_bfd ~file_name:name ~target
  @@
  let* _ = Bfd.set_object_format in
  let* sections = make_empty_sections sections in
  return ()
