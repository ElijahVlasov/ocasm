open! Import

module Mk (I : sig
  type instruction

  val byte_size : instruction -> int
end) =
struct
  open I

  type data_piece = {
    offset : int;
    command : (instruction, Directive.t) Preprocessed_command.t option;
  }

  type t = {
    mutable curr_offset : int;
    mutable curr_idx : int;
    data : data_piece Array.t;
    name : Section.t;
  }
  [@@deriving fields]

  let length content = content.curr_idx

  let bump content off =
    content.curr_offset <- content.curr_offset + off;
    content.curr_idx <- content.curr_idx + 1

  let add_dir content dir =
    content.data.(content.curr_idx) <-
      {
        offset = content.curr_offset;
        command = Option.some @@ Preprocessed_command.Directive dir;
      };
    bump content (Directive.byte_size dir)

  let add_instr content instr =
    content.data.(content.curr_idx) <-
      {
        offset = content.curr_offset;
        command = Option.some @@ Preprocessed_command.Instruction instr;
      };
    bump content (I.byte_size instr)

  let iter content ~f =
    for i = 0 to content.curr_idx - 1 do
      let elem = content.data.(i) in
      f (elem.offset, Option.value_exn elem.command)
    done

  let create capacity name =
    {
      name;
      curr_offset = 0;
      curr_idx = 0;
      data = Array.create ~len:capacity { offset = 0; command = None };
    }
end
