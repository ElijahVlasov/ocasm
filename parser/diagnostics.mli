open Base
open Ocasm_utils

exception Recoverable of (unit -> unit)
exception Non_recoverable

module Message : sig
  type t = {
    msg : string;
    id : int;
    left : int * int;
    right : int * int;
    file : Path.t;
    ctx : string;
  }
end

type t

val create : ?filter:int Hash_set.t -> ?promote:int Hash_set.t -> unit -> t
val error : ?recovery:(unit -> unit) -> t -> Message.t -> 'a
val warn : ?recovery:(unit -> unit) -> t -> Message.t -> unit
