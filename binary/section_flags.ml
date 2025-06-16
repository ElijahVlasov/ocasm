type t = int32

let equal x y = x = y
let pp = Base.Int32.pp
let sec_no_flags = C.Types.sec_no_flags
let sec_alloc = C.Types.sec_alloc
let sec_load = C.Types.sec_load
let sec_reloc = C.Types.sec_reloc
let sec_readonly = C.Types.sec_readonly
let sec_code = C.Types.sec_code
let sec_data = C.Types.sec_data
let sec_rom = C.Types.sec_rom
let sec_constructor = C.Types.sec_constructor
let sec_has_contents = C.Types.sec_has_contents
let sec_never_load = C.Types.sec_never_load
let sec_coff_shared_library = C.Types.sec_coff_shared_library
let sec_is_common = C.Types.sec_is_common
let sec_debugging = C.Types.sec_debugging
let sec_in_memory = C.Types.sec_in_memory
let sec_exclude = C.Types.sec_exclude
let sec_sort_entries = C.Types.sec_sort_entries
let sec_link_once = C.Types.sec_link_once
let sec_link_duplicates = C.Types.sec_link_duplicates
let sec_link_duplicates_discard = C.Types.sec_link_duplicates_discard
let sec_link_duplicates_one_only = C.Types.sec_link_duplicates_one_only
let sec_link_duplicates_same_size = C.Types.sec_link_duplicates_same_size

let sec_link_duplicates_same_contents =
  C.Types.sec_link_duplicates_same_contents

let sec_linker_created = C.Types.sec_linker_created
let ( |+ ) = Int32.logor
let to_int32 = Fun.id
