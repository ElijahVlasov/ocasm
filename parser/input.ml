open Base
open Ocasm_utils

module type C0 = sig
  type t

  val next : t -> char option
  val step : t -> bool
  val step_unchecked : t -> unit
  val back : t -> bool
  val back_unchecked : t -> unit
  val get : t -> char
end

module type C = sig
  include C0

  type input

  val create : input -> t
end

module type S = sig
  type t

  val next : t -> char
  val peek : t -> char
  val skip : t -> unit
  val close : t -> unit

  module Cursor : C with type input := t

  val advance : t -> Cursor.t -> unit
end

type 'a t = (module S with type t = 'a)

let eof = '\x00'

module StringInput = struct
  type t = { content : string; mutable cursor : int }

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
    type input = t
    type t = { parent : input; mutable pos : int }

    let create parent = { parent; pos = parent.cursor }
    let get i = String.unsafe_get i.parent.content i.pos

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

  let advance st i =
    let open Cursor in
    st.cursor <- i.pos
end

module FileInput = struct
  type t = {
    mutable buf1 : bytes;
    mutable buf2 : bytes;
    mutable cursor : int;
    mutable file_pos : int;
    in_ch : Stdlib.In_channel.t;
  }

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

    type input = t
    type t = { parent : input; pos : Pointer.t }

    let create parent = { parent; pos = ref parent.cursor }

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

module MakePositioned (Input : S) = struct
  module PF = Positioned.MakePositionedForward (Input)
  include PF

  let peek st = Input.peek (unwrap st)

  let next st =
    let ch = Input.next (unwrap st) in
    PF.step st ch;
    ch

  let skip st = Fn.ignore @@ next st
  let create parent = create parent
  let close st = Input.close (unwrap st)

  module Cursor = struct
    include Positioned.MakePositioned (Input.Cursor)
    module C = Input.Cursor

    let create parent =
      let parent_csr = C.create (PF.unwrap parent) in
      let line = PF.line parent in
      let col = PF.col parent in
      create parent_csr ~line ~col

    let get i = C.get (unwrap i)

    let next (i : t) =
      let ch = get i in
      let mb_next = C.next (unwrap i) in
      if Option.is_some mb_next then step i ch;
      mb_next

    let step_unchecked i =
      let ch = get i in
      step i ch;
      C.step_unchecked (unwrap i)

    let step i =
      let ch = get i in
      let has_stepped = C.step (unwrap i) in
      if has_stepped then step i ch;
      has_stepped

    let back_unchecked i =
      C.back_unchecked (unwrap i);
      back i (get i)

    let back i =
      let has_backed = C.back (unwrap i) in
      if has_backed then back i (get i);
      has_backed
  end

  let advance st i =
    let open Cursor in
    PF.set_pos st (line i) (col i);
    Input.advance (PF.unwrap st) (unwrap i)
end
