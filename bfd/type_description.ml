open Ctypes

module Types (F : TYPE) = struct
  open F

  let bfd_arch_size = constant "BFD_ARCH_SIZE" size_t
  let bfd_init_magic = constant "BFD_INIT_MAGIC" size_t

  type bfdm
  type bfd = bfdm Ctypes.structure

  let bfd : bfd structure typ = structure "bfd"
  let bfd_filename = field bfd "filename" string
  let bfd_section_count = field bfd "section_count" int
end
