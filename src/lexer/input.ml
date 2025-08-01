open Import
include Input_intf

module MkS (S0 : S0) = struct
  include S0

  let skip st =
    let (_ : char) = next st in
    ()

  let next_n_times ~n st = Fn.apply_n_times ~n (fun _ -> next st) '\x00'
  let skip_n_times ~n st = Fn.apply_n_times ~n (fun _ -> skip st) ()

  let rec read_buf st buf =
    let next_ch = next st in
    if not @@ Char.is_eof next_ch then (
      Buffer.add_char buf next_ch;
      read_buf st buf)
end

module StringInput0 = struct
  type t = { content : string; mutable cursor : int }

  let path _ = Path.empty

  let peek st =
    if st.cursor < String.length st.content then
      String.unsafe_get st.content st.cursor
    else '\x00'

  let next st =
    let next_ch = peek st in
    st.cursor <- st.cursor + 1;
    next_ch

  let create content = { content; cursor = 0 }
  let close st = ()
end

module StringInput = struct
  include StringInput0
  include MkS (StringInput0)
end

module FileInput0 = struct
  type t = {
    mutable buf1 : bytes;
    mutable buf2 : bytes;
    mutable cursor : int;
    mutable path_pos : int;
    path : Path.t;
    in_ch : In_channel.t;
  }
  [@@deriving fields]

  let chunk_len = 4096

  let read st buf =
    let module I = In_channel in
    let read = I.input st.in_ch buf 0 chunk_len in
    if read < chunk_len then (
      (* we have hit the eof *)
      st.path_pos <- -1;
      (* let's mark the eof in the buffer *)
      Bytes.set buf read '\x00')
    else st.path_pos <- st.path_pos + read

  let fetch_chunk st =
    (* If we have already hit the eof *)
    (* just move on to the next buffer *)
    if st.path_pos = -1 then st.buf1 <- st.buf2
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
    let next_ch = peek st in
    if not @@ Char.is_eof next_ch then
      if st.cursor = chunk_len - 1 then (
        fetch_chunk st;
        st.cursor <- 0)
      else st.cursor <- st.cursor + 1;
    next_ch

  let create path =
    let st =
      {
        buf1 = Bytes.create chunk_len;
        buf2 = Bytes.create chunk_len;
        cursor = 0;
        path_pos = 0;
        path;
        in_ch = In_channel.open_text (path :> string);
      }
    in
    read st st.buf1;
    read st st.buf2;
    st

  let close st = In_channel.close st.in_ch
end

module FileInput = struct
  include MkS (FileInput0)
  include FileInput0
end

let with_input (type a) inp_m inp ~f =
  let module I = (val inp_m : S with type t = a) in
  Exn.protect ~finally:(fun () -> I.close inp) ~f:(fun () -> f inp)

module MakePositioned0 (Input : S) = struct
  type t = Input.t Positioned.t

  open Positioned

  let create parent = create parent

  let next st =
    let ch = Input.next (unwrap st) in
    step st ch;
    ch

  let path = Positioned.with_value Input.path
  let peek = Positioned.with_value Input.peek
  let close = Positioned.with_value Input.close
end

module MakePositioned (Input : S) = struct
  include MkS (MakePositioned0 (Input))
  include MakePositioned0 (Input)
end
