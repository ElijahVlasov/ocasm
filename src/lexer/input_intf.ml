open! Import

(** Minimal signature of an input type. An input type allows to move forward
    through the input file and peek the next symbol. *)
module type S0 = sig
  type t

  val path : t -> Path.t
  (** The path to the file the input is observing. *)

  val next : t -> char
  (** The next symbol in the input. Mutates the state of the input type object.
      If the end of file has been hit returns ['\x00']. *)

  val peek : t -> char
  (** Gets the next symbol in the input but does not mutate the state. *)

  val close : t -> unit
  (** Close the input file or release the resource in some other way. *)
end

(** Full input sinature. Extends {!module-type:S0} with some useful functions.
*)
module type S = sig
  include S0

  val skip : t -> unit
  (** Consumes the next character but ignores the value. *)

  val next_n_times : n:int -> t -> char
  (** Consume an input char [n] times and return the last one. *)

  val skip_n_times : n:int -> t -> unit
  (** Consume an input char [n] times and ignore the values. *)

  val read_buf : t -> Buffer.t -> unit
  (** Read the input entirely into a buffer. *)
end

type 'a t = (module S with type t = 'a)

module type Intf = sig
  module type S0 = S0
  (** Minimal signature of an input type. An input type allows to move forward
      through the input file and peek the next symbol. *)

  module type S = S
  (** Full input sinature. Extends {!module-type:S0} with some useful functions.
  *)

  module MkS (S0 : S0) : S with type t = S0.t

  type 'a t = (module S with type t = 'a)
  (** Module type of an input module. *)

  module MakePositioned (Input : S) : sig
    type t = Input.t Positioned.t

    include S with type t := t

    val create : Input.t -> t
  end
end
