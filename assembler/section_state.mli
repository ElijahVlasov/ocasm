open Base
module Buffer = Stdlib.Buffer

type t

exception DuplicateSections of Section.t list

val create : Section.t list -> t
val finalize : t -> (Section.t, Buffer.t) Hashtbl.t
val write_byte : t -> char -> unit
val write_int32 : t -> int32 -> unit
val write_int64 : t -> int64 -> unit
val curr_section : t -> Section.t
val switch_section : t -> Section.t -> unit
val loc_counter : t -> int
val inc_loc_counter : t -> int -> unit
