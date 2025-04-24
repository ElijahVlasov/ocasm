open Base.Exn
open Ctypes
module Types = Types_generated

let _ =
  let ret_val = C.Functions.bfd_init () in
  if Unsigned.Size_t.equal ret_val C.Types.bfd_init_magic then ()
  else failwith "Failed to initialize bfd"

type bfd = C.Types.bfd structure ptr

let with_bfd (name : string) (target : string) (f : bfd -> 'a) =
  Printf.printf "hi";
  let bfd = C.Functions.bfd_openw name target in
  protect
    ~finally:(fun x ->
      if C.Functions.bfd_close bfd then () else failwith "Couldn't close file")
    ~f:(fun () -> f bfd)
