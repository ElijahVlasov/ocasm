open! Import

exception Recoverable of (unit -> unit)
exception Non_recoverable

module type S = sig
  type t

  val create :
    ?filter:(Diagnostics.Warning.t -> bool) ->
    ?promote:(Diagnostics.Warning.t -> bool) ->
    pp_err:(module Pretty_printer.S with type t = Diagnostics.Error.t) ->
    pp_warn:(module Pretty_printer.S with type t = Diagnostics.Warning.t) ->
    unit ->
    t

  val error :
    ?recovery:(unit -> unit) ->
    t ->
    Diagnostics.Error.t ->
    Diagnostics_context.t ->
    'a

  val warn :
    ?recovery:(unit -> unit) ->
    t ->
    Diagnostics.Warning.t ->
    Diagnostics_context.t ->
    unit
end

module type Intf = sig
  exception Recoverable of (unit -> unit)
  exception Non_recoverable

  module type S = S

  module Kitchen_sink_handler : sig
    include S

    val to_list : t -> (Diagnostics.Any.t * Diagnostics_context.t) list
  end

  module Std_handler : S
end
