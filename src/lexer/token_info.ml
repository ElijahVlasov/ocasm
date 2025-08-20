open! Import

type t = {
  starts : Location.t;
  ends : Location.t;
  string : unit -> string;
      [@equal fun x y -> String.equal (x ()) (y ())]
      [@printer fun fmt x -> fprintf fmt "%s" (x ())]
      (** A thunk of the physical representation of the token in the source
          file. *)
}
[@@deriving eq, show]

let to_string = show
