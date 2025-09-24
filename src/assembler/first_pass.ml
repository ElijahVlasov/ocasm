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
    symtab : (string, Section.t * int) Hashtbl.t;
    mutable curr_sec : Section.t;
  }
  [@@deriving fields]

  let add_instr st instr =
    (* let sec = Hashtbl.finds.(st.curr_sec) in *)
    Panic.unimplemented ()

  let add_directive st dir = Panic.unimplemented ()
  let switch_section st sec = Panic.unimplemented ()

  let consume st = function
    | Command.Instruction pseudoinstr ->
        List.iter ~f:(add_instr st) (prep_instruction pseudoinstr)
    | Command.Directive dir -> (
        match dir with
        | Directive.Section sec -> switch_section st sec
        | _ -> add_directive st dir)
    | Label label -> Panic.unimplemented ()

  let create () =
    {
      sections = Hashtbl.create (module Section);
      symtab = Hashtbl.create (module String);
      curr_sec = Section.of_string ".text";
    }
end
