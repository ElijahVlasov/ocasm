open! Import

module Mk (I : sig
  type pseudo_instruction
  type instruction

  val prep_instruction : pseudo_instruction -> instruction list
  val byte_size : instruction -> int
end) =
struct
  open I
  module Section_content = Section_content.Mk (I)

  type t = {
    sections : (Section.t, Section_content.t) Hashtbl.t;
    symtab : (string, Symbol.t) Hashtbl.t;
    capacity : int;
    mutable curr_sec : Section_content.t;
  }
  [@@deriving fields]

  let add_directive st = Section_content.add_dir st.curr_sec
  let add_instr st = Section_content.add_instr st.curr_sec

  let switch_section st sec =
    match Hashtbl.find st.sections sec with
    | None ->
        let new_section = Section_content.create st.capacity sec in
        Hashtbl.add_exn st.sections ~key:sec ~data:new_section;
        st.curr_sec <- new_section
    | Some content -> st.curr_sec <- content

  let consume st = function
    | Command.Instruction pseudoinstr ->
        List.iter ~f:(add_instr st) (prep_instruction pseudoinstr)
    | Command.Directive dir -> (
        match dir with
        | Directive.Section sec -> switch_section st sec
        | _ -> add_directive st dir)
    | Label label ->
        let open Symbol in
        let sym =
          {
            offset = Section_content.curr_offset st.curr_sec;
            section = Section_content.name st.curr_sec;
          }
        in
        Hashtbl.update st.symtab label ~f:(fun _ -> sym)

  let create capacity () =
    let text_section_name = Section.of_string ".text" in
    let text_section = Section_content.create capacity text_section_name in
    let sections = Hashtbl.create (module Section) in
    Hashtbl.add_exn sections ~key:text_section_name ~data:text_section;
    {
      sections;
      capacity;
      symtab = Hashtbl.create (module String);
      curr_sec = text_section;
    }
end
