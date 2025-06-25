type t

include Base.Equal.S with type t := t

val bsf_no_flags : t
val bsf_local : t
val bsf_global : t
val bsf_export : t
val bsf_debugging : t
val bsf_function : t
val bsf_keep : t
val bsf_elf_common : t
val bsf_weak : t
val bsf_section_sym : t
val bsf_old_common : t
val bsf_not_at_end : t
val bsf_constructor : t
val bsf_warning : t
val bsf_indirect : t
val bsf_file : t
val bsf_dynamic : t
val bsf_object : t
val bsf_debugging_reloc : t
val bsf_thread_local : t
val bsf_relc : t
val bsf_srelc : t
val bsf_synthetic : t
val bsf_gnu_indirect_function : t
val bsf_gnu_unique : t
val bsf_section_sym_used : t
val ( |+ ) : t -> t -> t
val to_int32 : t -> int32
