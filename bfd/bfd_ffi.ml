open Ctypes
open Foreign

let bfd_init = foreign "bfd_init" (void @-> returning void)
