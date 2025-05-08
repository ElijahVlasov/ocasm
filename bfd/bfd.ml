open Base
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
type asymbol = C.Types.asymbol structure ptr

module Section_flags = Section_flags
module Symbol_flags = Symbol_flags

module Error = struct
  include C.Types.Error

  let to_string = C.Functions.bfd_errmsg
  let get_error = C.Functions.bfd_get_error
  let no_error = equal C.Types.Error.NoError
end

exception BfdException of Error.t

let check_bfd_error (a : 'a) : 'a =
  let open Error in
  let err = get_error () in
  if no_error err then a else raise (BfdException err)

let close_file (bfd : bfd) =
  let _ = C.Functions.bfd_close bfd in
  check_bfd_error ()

let close_file_all_done (bfd : bfd) =
  let _ = C.Functions.bfd_close_all_done bfd in
  check_bfd_error ()

let close_file_all_done_unchecked (bfd : bfd) =
  let _ = C.Functions.bfd_close_all_done bfd in
  ()

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
  let ask = Fn.id
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
  protect
    ~finally:(fun () -> close_file_all_done_unchecked bfd)
    ~f:(fun () -> BfdMonad.run f bfd)

open BfdMonad

let ( let* ) x f = bind x ~f

let bfd_func_wrapper_0 (f : bfd -> 'a) : 'a BfdMonad.t =
  let* bfd = ask in
  let result = f bfd in
  return @@ check_bfd_error result

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

let bfd_pure_wrapper_2 (f : 'a -> 'b -> 'c) (a : 'a) (b : 'b) : 'c =
  check_bfd_error (f a b)

let set_object_format : unit BfdMonad.t =
  ignore_m @@ bfd_func_wrapper_1 C.Functions.bfd_set_format C.Types.bfd_object

let make_section : string -> asection BfdMonad.t =
  bfd_func_wrapper_1 C.Functions.bfd_make_section

let set_section_flags (section : asection) (flags : Section_flags.t) : unit =
  ignore
  @@ bfd_pure_wrapper_2 C.Functions.bfd_set_section_flags section
       (Section_flags.to_int32 flags)

let set_section_size (section : asection) (size : int64) : unit =
  ignore
  @@ bfd_pure_wrapper_2 C.Functions.bfd_set_section_size section
       (Unsigned.Size_t.of_int64 size)

let set_section_contents_raw =
  bfd_func_wrapper_4 C.Functions.bfd_set_section_contents

type 'a word_type = Word32 : int32 word_type | Word64 : int64 word_type
type to_ctype = CType : 'a typ * 'a list -> to_ctype

let ctype_and_list_of_word_type : type a. a word_type -> a list -> to_ctype =
 fun w l ->
  match w with Word32 -> CType (int32_t, l) | Word64 -> CType (int64_t, l)

let set_section_contents (type a) (witness : a word_type) (section : asection)
    (content : a list) (file_offset : int64) : unit BfdMonad.t =
  let (CType (typ, content)) = ctype_and_list_of_word_type witness content in
  let count = List.length content * sizeof typ in
  let count = Unsigned.Size_t.of_int count in
  let arr = CArray.of_list typ content in
  ignore_m
  @@ set_section_contents_raw section
       (to_voidp (CArray.start arr))
       file_offset count

let make_empty_symbol = bfd_func_wrapper_0 C.Functions.bfd_make_empty_symbol

let make_symbol (name : string) (sec : asection) (flags : Symbol_flags.t)
    (value : int64) : asymbol BfdMonad.t =
  let* sym = make_empty_symbol in
  setf !@sym C.Types.asym_name name;
  setf !@sym C.Types.asym_section sec;
  setf !@sym C.Types.asym_flags (Symbol_flags.to_int32 flags);
  setf !@sym C.Types.asym_value value;
  return @@ sym

let set_symtab_raw = bfd_func_wrapper_2 C.Functions.bfd_set_symtab

let set_symtab (syms : asymbol list) : unit BfdMonad.t =
  let len = List.length syms |> Unsigned.Size_t.of_int in
  let syms = List.append syms [ from_voidp C.Types.asymbol null ] in
  let arr = CArray.of_list (ptr C.Types.asymbol) syms in
  ignore_m @@ set_symtab_raw (CArray.start arr) len
