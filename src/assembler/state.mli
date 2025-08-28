open Base
open Utils

type 'a t

val create : 'a Word_type.t -> secs:Section.t list -> 'a t

val finalize :
  'a t -> (string, 'a Symbol.t) Hashtbl.t * (Section.t, Buffer.t) Hashtbl.t

val symtab_state : 'a t -> 'a Symtab_state.t
val section_state : 'a t -> Section_state.t
