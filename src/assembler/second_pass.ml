open! Import

module Mk (I : sig
  type instruction

  val register_relocations :
    (instruction, Directive.t) Preprocessed_command.t -> unit

  val assemble_instruction : bytes -> int -> instruction -> unit

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
  let assemble_directive section offset = function
    | Directive.Ascii strs ->
        let (_ : int) =
          List.fold strs ~init:offset ~f:(fun offset str ->
              let len = String.length str in
              Bytes.blit_string str 0 section offset len;
              offset + len)
        in
        ()
    | Directive.Asciiz strs ->
        let (_ : int) =
          List.fold strs ~init:offset ~f:(fun offset str ->
              let len = String.length str in
              Bytes.blit_string str 0 section offset len;
              Bytes.set section (offset + len) '\x00';
              offset + len + 1)
        in
        ()
    | Directive.Byte bs -> Bytes.blit bs 0 section offset (Bytes.length bs)
    | Directive.Skip n -> Bytes.fill section offset n '\x00'
    | Directive.Section _ ->
        Panic.unreachable ~msg:"Sections must've been removed by this moment" ()
    | Directive.Word32 words ->
        List.iteri words ~f:(fun i word ->
            Bytes.set_int32_le section (offset + (i * Int32.num_bits / 8)) word)

  let pass_section ~key:section ~data:section_data =
    let length = I.Section_content.length section_data in
    let bytes = Bytes.create length in
    I.Section_content.iter section_data ~f:(fun offset command ->
        I.register_relocations command;
        match command with
        | Preprocessed_command.Directive dir ->
            assemble_directive bytes offset dir
        | Preprocessed_command.Instruction instr ->
            I.assemble_instruction bytes offset instr);
    bytes

  let pass sections = Hashtbl.mapi sections ~f:pass_section
end
