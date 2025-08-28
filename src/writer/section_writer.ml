open Base
open Ocasm_bfd
open Ocasm_bfd.Bfd
open Ocasm_assembler
open Utils

type 'a section_info = { bfd_sec : Bfd.asection; contents : 'a list }

let ( let* ) = BfdMonad.( >>= )

let section_flags (sec : Section.t) =
  let open Section_flags in
  match sec with
  | Section.ROData -> sec_has_contents |+ sec_readonly
  | Section.Text -> sec_has_contents |+ sec_code
  | Section.Data -> sec_has_contents |+ sec_data
  | Section.Bss -> sec_no_flags

let init_sections wt sections =
  let open BfdMonad in
  map ~f:(Hashtbl.of_alist_exn (module Section))
  @@ all
  @@
  let mk_sec_info (section, contents) =
    let size = Int64.of_int @@ Word_type.sizeof_list wt contents in
    let* bfd_sec = Bfd.make_section (Section.to_string section) in
    let flags = section_flags section in
    Bfd.set_section_flags bfd_sec flags;
    Bfd.set_section_size bfd_sec size;
    return (section, { bfd_sec; contents })
  in
  List.map sections ~f:mk_sec_info

let write_section_contents wt sections =
  let open Bfd.BfdMonad in
  ignore_m @@ all
  @@
  let ( + ) = Int64.( + ) in
  let offset = ref 0L in
  List.map
    ~f:(fun (_, sec_info) ->
      let file_offset = !offset in
      offset :=
        file_offset + Int64.of_int (Word_type.sizeof_list wt sec_info.contents);
      Bfd.set_section_contents wt ~sec:sec_info.bfd_sec
        ~content:sec_info.contents ~file_offset)
    (Hashtbl.to_alist sections)
