open Base

exception Recoverable of (unit -> unit)
exception Non_recoverable

module type S = sig
  type t

  val create : ?filter:int Hash_set.t -> ?promote:int Hash_set.t -> unit -> t
  val throw : ?recovery:(unit -> unit) -> t -> 'a Diagnostic_message.t -> 'a
end

module type Intf = sig
  exception Recoverable of (unit -> unit)
  exception Non_recoverable

  module type S = S

  module Kitchen_sink_handler : sig
    include S

    val to_list : t -> Diagnostic_message.dyn list
  end

  module Std_handler : S
end
