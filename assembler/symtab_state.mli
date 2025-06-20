open Base
open Ocasm_binary

type 'a t

val create : 'a Word_type.t -> 'a t
val finalize : 'a t -> (string, 'a Symbol.t) Hashtbl.t

exception DuplicateSymbol of string
val add_symbol : 'a t -> 'a Symbol.t -> unit
