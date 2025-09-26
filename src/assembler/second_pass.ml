open! Import

module Mk (I : sig
  type instruction

  val register_relocations :
    (instruction, Directive.t) Preprocessed_command.t -> unit

  module Section_content : sig
    type t

    val length : t -> int

    val iter :
      t ->
      f:(int -> (instruction, Directive.t) Preprocessed_command.t -> unit) ->
      unit
  end
end) =
struct
  let assemble_directive section offset dir = ()
  let assemble_instruction section offset instr = ()

  let pass_section ~key:section ~data:section_data =
    let length = I.Section_content.length section_data in
    let bytes = Bytes.create length in
    I.Section_content.iter section_data ~f:(fun offset command ->
        match command with
        | Preprocessed_command.Directive dir ->
            assemble_directive bytes offset dir
        | Preprocessed_command.Instruction instr ->
            assemble_instruction bytes offset instr);
    bytes

  let pass sections = Hashtbl.mapi sections ~f:pass_section
end
