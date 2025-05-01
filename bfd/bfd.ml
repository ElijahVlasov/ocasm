open Base.Exn
open Ctypes
module Types = Types_generated

let _ =
  let ret_val = C.Functions.bfd_init () in
  if Unsigned.Size_t.equal ret_val C.Types.bfd_init_magic then ()
  else failwith "Failed to initialize bfd"

type bfd = C.Types.bfd structure ptr
type asection = C.Types.asection structure ptr

exception BfdException of C.Types.bfd_error_type

let check_bfd_error (a : 'a) : 'a =
  let err = C.Functions.bfd_get_error () in
  if C.Types.equal_bfd_error_type err C.Types.NoError then a
  else raise (BfdException err)

let protect_bfd_error (f : unit -> 'a) : 'a =
  let err = C.Functions.bfd_get_error () in
  if C.Types.equal_bfd_error_type err C.Types.NoError then f ()
  else raise (BfdException err)

let close_file (bfd : bfd) =
  let _ = C.Functions.bfd_close bfd in
  check_bfd_error ()

module BfdMonad : sig
  include Base.Monad.Basic

  val run : 'a t -> bfd -> 'a
  val ask : bfd t
end = struct
  type 'a t = bfd -> 'a

  let bind a ~f bfd = f (a bfd) bfd
  let return a _ = a
  let map = `Custom (fun a ~f bfd -> f (a bfd))
  let run a bfd = a bfd
  let ask = Base.Fn.id
end

let with_bfd (name : string) (target : string) (f : 'a BfdMonad.t) =
  let bfd = C.Functions.bfd_openw name target in
  protect_bfd_error (fun () ->
      protect
        ~finally:(fun () -> close_file bfd)
        ~f:(fun () -> BfdMonad.run f bfd))

module BfdMonadFull = Base.Monad.Make (BfdMonad)
open BfdMonad
open BfdMonadFull

let ( let* ) x f = bind x ~f
let ( let+ ) x f = map ~f x

let set_object_format : bool BfdMonad.t =
  let* bfd = ask in
  return (C.Functions.bfd_set_format bfd C.Types.bfd_object)

let make_section (name : string) : asection BfdMonad.t =
  let* bfd = ask in
  return (C.Functions.bfd_make_section bfd name)

let set_section_flags (section : asection) (flags : Section_flags.t) : bool =
  C.Functions.bfd_set_section_flags section (Section_flags.to_int32 flags)

let set_section_contents (section : asection) (content : 'a list)
    (file_offset : int64) (typ : 'a typ) : bool BfdMonad.t =
  let* bfd = ask in
  let count = List.length content * sizeof typ in
  let arr = CArray.of_list typ content in
  return
    (C.Functions.bfd_set_section_contents bfd section
       (to_voidp (CArray.start arr))
       file_offset
       (Unsigned.Size_t.of_int count))
