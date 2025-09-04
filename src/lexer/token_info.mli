open! Import

type t = {
  starts : Location.t;  (** Here the token starts *)
  ends : Location.t;  (** Here the token ends *)
  string : string;
      (** The physical representation of the token in the source file. *)
}

include Default.S with type t := t
include Equal.S with type t := t
include To_string.S with type t := t
include Pretty_printer.S with type t := t
