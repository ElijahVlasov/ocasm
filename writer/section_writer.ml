open Base
open Bfd
open Bfd.CArray
open Section

type 'a section_info = { bfd_sec : Bfd.asection; contents : 'a list }

let ( let* ) = BfdMonad.( >>= )

let init_sections word_type sections =
  let open BfdMonad in
  map ~f:(Hashtbl.of_alist_exn (module Section))
  @@ all
  @@
  let mk_sec_info (section, contents) =
    let size = Int64.of_int @@ WordType.sizeof_list word_type contents in
    let* bfd_sec = Bfd.make_section (Section.to_string section) in
    let flags = section_flags section in
    Bfd.set_section_flags bfd_sec flags;
    Bfd.set_section_size bfd_sec size;
    return (section, { bfd_sec; contents })
  in
  List.map sections ~f:mk_sec_info

let write_section_contents word_type sections =
  let open BfdMonad in
  ignore_m @@ all
  @@
  let ( + ) = Int64.( + ) in
  let offset = ref 0L in
  List.map
    ~f:(fun (_, sec_info) ->
      let file_offset = !offset in
      offset :=
        file_offset
        + Int64.of_int (WordType.sizeof_list word_type sec_info.contents);
      Bfd.set_section_contents word_type ~sec:sec_info.bfd_sec
        ~content:sec_info.contents ~file_offset)
    (Hashtbl.to_alist sections)
