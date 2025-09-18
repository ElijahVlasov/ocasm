open! Import

type t =
  | Ascii of string list
  | Asciiz of string list
  | Byte of bytes
  | Skip of int
  | Section of Section.t
  | Word of int32 list

(* let assemble_directive s d = *)
(*   let sec_s = State.section_state s in *)
(*   let stab_s = State.symtab_state s in *)
(*   match d with *)
(*   | Ascii strs -> List.iter strs ~f:(Section_state.write_string sec_s) *)
(*   | Asciiz strs -> *)
(*       let write_stringz str = *)
(*         Section_state.write_string sec_s str; *)
(*         Section_state.write_byte sec_s '\x00' *)
(*       in *)
(*       List.iter strs ~f:write_stringz *)
(*   | Byte bs -> Section_state.write_bytes sec_s bs *)
(*   | Skip n -> Section_state.inc_loc_counter sec_s n *)
(*   | Section sec -> Section_state.switch_section sec_s sec *)
(*   | Word ws -> List.iter ws ~f:(Section_state.write_int32 sec_s) *)
