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
      ( string,
        int * (instruction, Directive.t) Preprocessed_command.t )
      Hashtbl.t;
    symtab : (string, unit) Hashtbl.t;
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
