type t

include Base.Equal.S with type t := t
include Base.Pretty_printer.S with type t := t

val sec_no_flags : t
val sec_alloc : t
val sec_load : t
val sec_reloc : t
val sec_readonly : t
val sec_code : t
val sec_data : t
val sec_rom : t
val sec_constructor : t
val sec_has_contents : t
val sec_never_load : t
val sec_coff_shared_library : t
val sec_is_common : t
val sec_debugging : t
val sec_in_memory : t
val sec_exclude : t
val sec_sort_entries : t
val sec_link_once : t
val sec_link_duplicates : t
val sec_link_duplicates_discard : t
val sec_link_duplicates_one_only : t
val sec_link_duplicates_same_size : t
val sec_link_duplicates_same_contents : t
val sec_linker_created : t
val ( |+ ) : t -> t -> t
val to_int32 : t -> int32
