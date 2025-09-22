open! Import

module Mk (I : sig
  type pseudo_instruction
  type instruction

  val prep_instruction : pseudo_instruction -> instruction list
end) =
struct
  open I

  type t = {
    sections :
      ( Section.t,
        (int * (instruction, Directive.t) Preprocessed_command.t) Array.t )
      Hashtbl.t;
    symtab : (string, Section.t * int) Hashtbl.t;
    mutable curr_sec : string;
  }

  let consume st = function
    | Command.Instruction instr -> _
    | Command.Directive dir -> _
    | Label label -> _

  let create () =
    {
      sections = Hashtbl.create (module String);
      symtab = Hashtbl.create (module String);
      curr_sec = ".text";
    }
end
