open! Import

module type S = sig
  type t
  (** This type is supposed to be the type of ISA-specific tokens. I.e.
      registers, instruction names, etc. *)

  val directive : string -> t option
  (** Tries to parse a directive (something that starts with ['.']). *)

  val name : string -> t option
  (** Tries to parse an ISA-name (register, instruction, etc). *)

  val reserved : string -> t option
  (** Tries to parse a resrved keyword (something that starts with ['%']). *)

  include To_string.S with type t := t
  include Equal.S with type t := t
end

type 'a t = (module S with type t = 'a)

module type Intf = sig
  module type S = S

  type 'a t = (module S with type t = 'a)

  module OF_ISA (I : Isa.S) :
    S
      with type t =
        (I.Register.t, I.Directive.t, I.Opcode.t, I.Reserved.t) Isa.Token.t
end
