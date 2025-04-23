open Ctypes

module Types (F : TYPE) = struct
  open F

  let bfd_arch_size = constant "BFD_ARCH_SIZE" int

  type bfd

  let bfd : bfd structure typ = structure "bfd"
  let bfd_filename = field bfd "filename" string
  let bfd_section_count = field bfd "section_count" int
end
