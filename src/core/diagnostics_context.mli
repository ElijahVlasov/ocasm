open! Import
include module type of Diagnostics_context_intf

val pp : Formatter.t -> t -> unit
