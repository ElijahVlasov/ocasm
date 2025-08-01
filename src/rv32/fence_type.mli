type fence_type

val i : fence_type
val o : fence_type
val r : fence_type
val w : fence_type
val equal_fence_type : fence_type -> fence_type -> bool
val show_fence_type : fence_type -> string
val pp_fence_type : Format.formatter -> fence_type -> unit
val ( |+ ) : fence_type -> fence_type -> fence_type
val to_int32 : fence_type -> int32
