open Base

module type S = sig
  type t

  val next : t -> char
  val peek : t -> char
  val skip : t -> unit
  val close : t -> unit
end

type 'a t = (module S with type t = 'a)

let eof = '\x00'

module StringInput : sig
  include S

  val create : content:string -> t
end = struct
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
end

module FileInput : sig
  include S

  val create : path:string -> t
end = struct
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
    let ( = ) = Char.equal in
    let next_ch = peek st in
    (if next_ch = eof then ()
     else
       let ( = ) = Int.equal in
       if st.cursor = chunk_len - 1 then (
         fetch_chunk st;
         st.cursor <- 0)
       else st.cursor <- st.cursor + 1);
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
end

let with_input : type a b. a t -> a -> f:(a -> b) -> b =
 fun inp_m inp ~f ->
  let module I = (val inp_m : S with type t = a) in
  Exn.protect ~finally:(fun () -> I.close inp) ~f:(fun () -> f inp)

module MakePos (Input : S) : sig
  include S

  val create : Input.t -> t
  val pos : t -> int * int
end = struct
  type t = { wrapped : Input.t; mutable line : int; mutable col : int }

  let peek st = Input.peek st.wrapped

  let next st =
    let ( = ) = Char.equal in
    let next_line st =
      st.line <- st.line + 1;
      st.col <- 0
    in
    let ch = Input.next st.wrapped in
    if ch = '\n' then next_line st else st.col <- st.col + 1;
    ch

  let skip st = Fn.ignore @@ next st
  let pos st = (st.line, st.col)
  let create wrapped = { wrapped; line = 0; col = 0 }
  let close st = Input.close st.wrapped
end
