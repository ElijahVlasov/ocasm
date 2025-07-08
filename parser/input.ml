open Base
open Ocasm_utils

module type C = sig
  type t

  val next : t -> char option
  val step : t -> bool
  val step_unchecked : t -> unit
  val back : t -> bool
  val back_unchecked : t -> unit
  val get : t -> char
end

module type S = sig
  type t

  val next : t -> char
  val peek : t -> char
  val skip : t -> unit
  val close : t -> unit

  module Cursor : C

  val parent : Cursor.t -> t
  val start : t -> Cursor.t
  val advance : t -> Cursor.t -> unit
end

type 'a t = (module S with type t = 'a)

let eof = '\x00'

type string_input = { content : string; mutable cursor : int }

module StringInput : sig
  include S

  val create : content:string -> t
end = struct
  type t = string_input

  let peek st =
    if st.cursor < String.length st.content then
      String.unsafe_get st.content st.cursor
    else eof

  let next st =
    let next_ch = peek st in
    st.cursor <- st.cursor + 1;
    next_ch

  let skip st = Fn.ignore @@ next st
  let create ~content = { content; cursor = 0 }
  let close st = ()

  module Cursor = struct
    type t = { parent : string_input; mutable pos : int }

    let parent i = i.parent
    let create parent pos = { parent; pos }
    let get i = String.unsafe_get i.parent.content i.pos
    let pos i = i.pos

    let next i =
      let next_pos = i.pos + 1 in
      if next_pos = String.length i.parent.content then None
      else (
        i.pos <- next_pos;
        Some (get i))

    let step i = Option.is_some @@ next i

    let back i =
      let is_parent_cursor = i.pos <> i.parent.cursor in
      if is_parent_cursor then i.pos <- i.pos - 1;
      is_parent_cursor

    let step_unchecked i = i.pos <- i.pos + 1
    let back_unchecked i = i.pos <- i.pos - 1
  end

  let parent = Cursor.parent
  let start st = Cursor.create st st.cursor
  let advance st i = st.cursor <- Cursor.pos i
end

type file_input = {
  mutable buf1 : bytes;
  mutable buf2 : bytes;
  mutable cursor : int;
  mutable file_pos : int;
  in_ch : Stdlib.In_channel.t;
}

module FileInput : sig
  include S

  val create : path:string -> t
end = struct
  type t = file_input

  let chunk_len = 4096

  let read st buf =
    let module I = Stdlib.In_channel in
    let read = I.input st.in_ch buf 0 chunk_len in
    if read < chunk_len then (
      (* we have hit the eof *)
      st.file_pos <- -1;
      (* let's mark the eof in the buffer *)
      Bytes.set buf read eof)
    else st.file_pos <- st.file_pos + read

  let fetch_chunk st =
    let ( = ) = Int.equal in
    (* If we have already hit the eof *)
    (* just move on to the next buffer *)
    if st.file_pos = -1 then st.buf1 <- st.buf2
    else
      let swp_bufs st =
        let tmp = st.buf1 in
        st.buf1 <- st.buf2;
        st.buf2 <- tmp
      in
      swp_bufs st;
      read st st.buf2

  let peek st = Bytes.unsafe_get st.buf1 st.cursor

  let next st =
    let ( <> ) = Char.( <> ) in
    let next_ch = peek st in
    if next_ch <> eof then
      if st.cursor = chunk_len - 1 then (
        fetch_chunk st;
        st.cursor <- 0)
      else st.cursor <- st.cursor + 1;
    next_ch

  let skip st = Fn.ignore @@ next st

  let create ~path =
    let module I = Stdlib.In_channel in
    let st =
      {
        buf1 = Bytes.create chunk_len;
        buf2 = Bytes.create chunk_len;
        cursor = 0;
        file_pos = 0;
        in_ch = I.open_text path;
      }
    in
    read st st.buf1;
    read st st.buf2;
    st

  let close st = Stdlib.In_channel.close st.in_ch

  module Cursor = struct
    module Pointer = struct
      type t = int ref

      let max_ptr = (chunk_len * 2) - 1
      let is_last i = !i = max_ptr
      let is_out_of_bounds i = !i = max_ptr + 1
      let inc_unsafe i = i := !i + 1
      let inc i = if not (is_last i) then inc_unsafe i
      let dec_unsafe i = i := !i - 1
      let dec i = if !i <> 0 then dec_unsafe i
      let buffer_flag_mask = chunk_len
      let buffer_ind_mask = chunk_len - 1
      let is_second_buffer i = !i land buffer_flag_mask <> 0
      let is_first_buffer i = !i land buffer_flag_mask = 0
      let ind i = !i land buffer_ind_mask
    end

    type t = { parent : file_input; pos : Pointer.t }

    let parent i = i.parent
    let pos i = i.pos
    let create parent pos = { parent; pos = ref pos }

    let get i =
      let buf =
        if Pointer.is_first_buffer i.pos then i.parent.buf1 else i.parent.buf2
      in
      let ind = Pointer.ind i.pos in
      Bytes.unsafe_get buf ind

    let is_last i = Pointer.is_last i.pos

    let next i =
      if is_last i then None
      else
        let ch = get i in
        let ( = ) = Char.equal in
        if ch = eof then None
        else (
          Pointer.inc_unsafe i.pos;
          let ch = get i in
          if ch = eof then (
            Pointer.dec_unsafe i.pos;
            None)
          else Some ch)

    let step i = Option.is_some @@ next i
    let step_unchecked i = Pointer.inc_unsafe i.pos

    let back i =
      let is_parent_cursor = !(i.pos) <> i.parent.cursor in
      if is_parent_cursor then Pointer.dec i.pos;
      is_parent_cursor

    let back_unchecked i = Pointer.dec_unsafe i.pos
  end

  let parent = Cursor.parent
  let start st = Cursor.create st st.cursor

  let advance st i =
    let open Cursor in
    let open Cursor.Pointer in
    let ind = ind i.pos in
    if is_second_buffer i.pos then fetch_chunk st;
    st.cursor <- ind
end

let with_input (type a) inp_m inp ~f =
  let module I = (val inp_m : S with type t = a) in
  Exn.protect ~finally:(fun () -> I.close inp) ~f:(fun () -> f inp)

type 'a positioned_input = {
  wrapped : 'a;
  mutable line : int;
  mutable col : int;
}

module MakePos (Input : S) : sig
  include S

  val create : Input.t -> t
  val pos : t -> int * int
end = struct
  type t = Input.t positioned_input

  let peek st = Input.peek st.wrapped

  let next st =
    let ( = ) = Char.equal in
    let next_line st =
      st.line <- st.line + 1;
      st.col <- 0
    in
    let ch = Input.next st.wrapped in
    if Char.is_newline ch then next_line st else st.col <- st.col + 1;
    ch

  let skip st = Fn.ignore @@ next st
  let pos st = (st.line, st.col)
  let create wrapped = { wrapped; line = 0; col = 0 }
  let close st = Input.close st.wrapped

  module Cursor = struct
    type t = {
      wrapped : Input.Cursor.t;
      parent : Input.t positioned_input;
      mutable line : int;
      mutable col : int;
    }

    let parent i = i.parent
    let create wrapped parent line col = { wrapped; parent; line; col }
    let unwrap i = i.wrapped
    let line i = i.line
    let col i = i.col

    let get (i : t) =
      let open Input in
      Cursor.get i.wrapped

    let newline i =
      i.col <- 0;
      i.line <- i.line + 1

    let next_col i = i.col <- i.col + 1

    let next (i : t) =
      let ch = get i in
      let open Input in
      match Cursor.next i.wrapped with
      | Some _ as res ->
          if Char.is_newline ch then newline i else next_col i;
          res
      | x -> x

    let step i =
      let ch = get i in
      if Char.is_newline ch then
        if Input.Cursor.step i.wrapped then (
          newline i;
          true)
        else false
      else Input.Cursor.step i.wrapped

    let step_unchecked i =
      let ch = get i in
      if Char.is_newline ch then newline i;
      Input.Cursor.step_unchecked i.wrapped

    let back i =
      if Input.Cursor.back i.wrapped then (
        if Char.is_newline (get i) then (
          i.col <- 0;
          i.line <- i.line - 1)
        else i.col <- i.col - 1;
        true)
      else false

    let back_unchecked i =
      Input.Cursor.back_unchecked i.wrapped;
      if Char.is_newline (get i) then (
        i.col <- 0;
        i.line <- i.line - 1)
      else i.col <- i.col - 1
  end

  let parent = Cursor.parent
  let start st = Cursor.create (Input.start st.wrapped) st st.line st.col

  let advance st i =
    st.line <- Cursor.line i;
    st.col <- Cursor.col i;
    Input.advance st.wrapped (Cursor.unwrap i)
end
