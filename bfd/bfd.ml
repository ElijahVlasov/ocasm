open Base.Exn
open Ctypes
module Types = Types_generated

let _ =
  let ( = ) = Unsigned.Size_t.equal in
  let ret_val = C.Functions.bfd_init () in
  if ret_val = C.Types.bfd_init_magic then ()
  else failwith "Failed to initialize bfd"

type bfd = C.Types.bfd structure ptr
type asection = C.Types.asection structure ptr

exception BfdException of C.Types.bfd_error_type

let check_bfd_error (a : 'a) : 'a =
  let ( = ) = C.Types.equal_bfd_error_type in
  let err = C.Functions.bfd_get_error () in
  if err = C.Types.NoError then a else raise (BfdException err)

let protect_bfd_error (f : unit -> 'a) : 'a =
  let ( = ) = C.Types.equal_bfd_error_type in
  let res = f () in
  let err = C.Functions.bfd_get_error () in
  if err = C.Types.NoError then res else raise (BfdException err)

let close_file (bfd : bfd) =
  let _ = C.Functions.bfd_close bfd in
  check_bfd_error ()

let close_file_all_done (bfd : bfd) =
  let _ = C.Functions.bfd_close_all_done bfd in
  check_bfd_error ()

module BfdMonadBasic : sig
  include Base.Monad.Basic

  val run : 'a t -> bfd -> 'a
  val ask : bfd t
end = struct
  type 'a t = bfd -> 'a

  let bind a ~f bfd = f (a bfd) bfd
  let return a _ = a
  let map = `Custom (fun a ~f bfd -> f (a bfd))
  let run a bfd = a bfd
  let ask = Fun.id
end

module BfdMonad : sig
  type 'a t

  include Base.Monad.S with type 'a t := 'a t

  val run : 'a t -> bfd -> 'a
  val ask : bfd t
end = struct
  type 'a t = 'a BfdMonadBasic.t

  include Base.Monad.Make (BfdMonadBasic)

  let run = BfdMonadBasic.run
  let ask = BfdMonadBasic.ask
end

let with_bfd (name : string) (target : string) (f : 'a BfdMonad.t) =
  let bfd = C.Functions.bfd_openw name target in
  protect_bfd_error (fun () ->
      protect
        ~finally:(fun () -> close_file_all_done bfd)
        ~f:(fun () -> BfdMonad.run f bfd))

open BfdMonad

let ( let* ) x f = bind x ~f
let ( let+ ) x f = map ~f x
let error_msg : C.Types.bfd_error_type -> string = C.Functions.bfd_errmsg

let bfd_func_wrapper_1 (f : bfd -> 'a -> 'b) (a : 'a) : 'b BfdMonad.t =
  let* bfd = ask in
  let result = f bfd a in
  return @@ check_bfd_error result

let bfd_func_wrapper_2 (f : bfd -> 'a -> 'b -> 'c) (a : 'a) (b : 'b) :
    'c BfdMonad.t =
  let* bfd = ask in
  let result = f bfd a b in
  return @@ check_bfd_error result

let bfd_func_wrapper_3 (f : bfd -> 'a -> 'b -> 'c -> 'd) (a : 'a) (b : 'b)
    (c : 'c) : 'd BfdMonad.t =
  let* bfd = ask in
  let result = f bfd a b c in
  return @@ check_bfd_error result

let bfd_func_wrapper_4 (f : bfd -> 'a -> 'b -> 'c -> 'd -> 'e) (a : 'a) (b : 'b)
    (c : 'c) (d : 'd) : 'e BfdMonad.t =
  let* bfd = ask in
  let result = f bfd a b c d in
  return @@ check_bfd_error result

let set_object_format : bool BfdMonad.t =
  bfd_func_wrapper_1 C.Functions.bfd_set_format C.Types.bfd_object

let make_section : string -> asection BfdMonad.t =
  bfd_func_wrapper_1 C.Functions.bfd_make_section

let set_section_flags (section : asection) (flags : Section_flags.t) : bool =
  C.Functions.bfd_set_section_flags section (Section_flags.to_int32 flags)

let set_section_size (section : asection) (size : int64) : bool =
  C.Functions.bfd_set_section_size section (Unsigned.Size_t.of_int64 size)

let set_section_contents (section : asection) (content : 'a list)
    (file_offset : int64) (typ : 'a typ) : bool BfdMonad.t =
  let count = List.length content * sizeof typ in
  let count = Unsigned.Size_t.of_int count in
  let arr = CArray.of_list typ content in
  bfd_func_wrapper_4 C.Functions.bfd_set_section_contents section
    (to_voidp (CArray.start arr))
    file_offset count
