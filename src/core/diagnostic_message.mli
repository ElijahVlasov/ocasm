open! Import
include module type of Diagnostic_message_intf

module Diagnostic_type : sig
  type 'a t = 'a diagnostic_type

  val pp : Formatter.t -> 'a t -> unit
end

module Dyn : sig
  type t = dyn

  include Equal.S with type t := t

  val pp : Formatter.t -> t -> unit
end

val pp : Formatter.t -> 'a t -> unit
