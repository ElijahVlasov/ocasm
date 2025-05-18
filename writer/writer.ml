open Base
open Bfd.CArray

module Section : sig
  type t = Data | Text | Bss [@@deriving eq, ord, hash, sexp]

  include Equal.S with type t := t

  val to_string : t -> string
end = struct
  type t = Data | Text | Bss [@@deriving eq, ord, hash, sexp]

  let to_string = function Data -> ".data" | Text -> ".text" | Bss -> ".bss"
end

type 'a section_info = { bfd_sec : Bfd.asection; contents : 'a list }

type 'a symbol = {
  section : Section.t;
  name : string;
  value : 'a;
  flags : Bfd.Symbol_flags.t;
}

open Bfd.BfdMonad

let ( let* ) = ( >>= )

let section_flags (section : Section.t) : Bfd.Section_flags.t =
  let open Bfd.Section_flags in
  match section with
  | Section.Data -> sec_has_contents |+ sec_data
  | Section.Text -> sec_has_contents |+ sec_code
  | Section.Bss -> sec_alloc

let init_sections : type a.
    a word_type ->
    (Section.t * a list) list ->
    (Section.t, a section_info) Hashtbl.t t =
 fun word_type sections ->
  map ~f:(Hashtbl.of_alist_exn (module Section))
  @@ all
  @@
  let mk_sec_info (section, contents) =
    let size = Int64.of_int @@ sizeof_list word_type contents in
    let* bfd_sec = Bfd.make_section (Section.to_string section) in
    let flags = section_flags section in
    Bfd.set_section_flags bfd_sec flags;
    Bfd.set_section_size bfd_sec size;
    return (section, { bfd_sec; contents })
  in
  List.map sections ~f:mk_sec_info

let set_symtab : type a.
    (Section.t, a section_info) Hashtbl.t -> a symbol list -> unit t =
 fun sections symtab ->
  let* symtab =
    all
    @@
    let mk_symbol ({ section; name; value; flags } : a symbol) =
      let sec = (Hashtbl.find_exn sections section).bfd_sec in
      Bfd.make_symbol ~sec ~name ~flags ~value
    in
    List.map ~f:mk_symbol symtab
  in
  Bfd.set_symtab symtab

let write_section_contents : type a.
    a word_type -> (Section.t, a section_info) Hashtbl.t -> unit t =
 fun word_type sections ->
  ignore_m @@ all
  @@
  let ( + ) = Int64.( + ) in
  let offset = ref 0L in
  List.map
    ~f:(fun (_, sec_info) ->
      let file_offset = !offset in
      offset :=
        file_offset + Int64.of_int (sizeof_list word_type sec_info.contents);
      Bfd.set_section_contents word_type ~sec:sec_info.bfd_sec
        ~content:sec_info.contents ~file_offset)
    (Hashtbl.to_alist sections)

let write_object_file ~(word_type : 'a word_type) ~(name : string)
    ~(sections : (Section.t * 'a list) list) ~(symtab : 'a symbol list) =
  let target = "elf32-bigriscv" in
  Bfd.with_bfd ~file_name:name ~target
  @@
  let* _ = Bfd.set_object_format in
  let* sections = init_sections word_type sections in
  let* _ = set_symtab sections symtab in
  write_section_contents word_type sections
