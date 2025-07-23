open Base
open Ocasm_utils

module Buffer_like = struct
  module type S = sig
    type t

    val add_char : t -> char -> unit
    val contents : t -> string
    val lc_contents : t -> string
    val to_number : t -> int64 array
    val clear : t -> unit
  end

  module Buffer : sig
    type t = Buffer.t

    include S with type t := t
    include module type of Buffer with type t := t
  end = struct
    include Buffer

    let to_number _ = Panic.unreachable ()
    let lc_contents _ = Panic.unreachable ()
    let to_case_sensitive _ = Panic.unreachable ()
  end

  module Lc_buffer : sig
    include S

    val create : int -> t
    val to_case_sensitive : t -> Buffer.t
  end = struct
    type t = { contents : Buffer.t; lc_contents : Buffer.t }

    let create n = { contents = Buffer.create n; lc_contents = Buffer.create n }

    let add_char b c =
      Buffer.add_char b.contents c;
      Buffer.add_char b.lc_contents (Char.lowercase c)

    let clear b =
      Buffer.clear b.contents;
      Buffer.clear b.lc_contents

    let contents b = Buffer.contents b.contents
    let lc_contents b = Buffer.contents b.lc_contents
    let to_number _ = Panic.unreachable ()
    let to_case_sensitive b = b.contents
  end

  module MkNumber_builder (Bldr : Number_builder.S) : sig
    include S

    val create : Bldr.t -> Buffer.t -> t
  end = struct
    type t = { bldr : Bldr.t; contents : Buffer.t }

    let create bldr contents = { bldr; contents }

    let add_char buf ch =
      Buffer.add_char buf.contents ch;
      Bldr.add_char buf.bldr ch

    let contents buf = Buffer.contents buf.contents
    let lc_contents buf = Panic.unreachable ()

    let clear buf =
      Bldr.clear buf.bldr;
      Buffer.clear buf.contents

    let to_number buf = Bldr.build buf.bldr
    let to_case_sensitive buf = Panic.unreachable ()
  end
end

type case_insensitive = Buffer_like.Lc_buffer.t
type case_sensitive = Buffer_like.Buffer.t
type 'a number_builder = 'a

module Buffer = struct
  type 'a t = { buf_m : (module Buffer_like.S with type t = 'a); buf : 'a }

  let add_char (type a) b ch =
    let module B = (val b.buf_m : Buffer_like.S with type t = a) in
    B.add_char b.buf ch

  let contents (type a) b =
    let module B = (val b.buf_m : Buffer_like.S with type t = a) in
    B.contents b.buf

  let lc_contents (b : case_insensitive t) =
    let module B = (val b.buf_m : Buffer_like.S with type t = case_insensitive)
    in
    B.lc_contents b.buf

  let to_number (type a) (b : a number_builder t) =
    let module B = (val b.buf_m : Buffer_like.S with type t = a) in
    B.to_number b.buf

  let clear (type a) b =
    let module B = (val b.buf_m : Buffer_like.S with type t = a) in
    B.clear b.buf

  let with_buf buf f =
    clear buf;
    f buf
end

module Bin_builder = Buffer_like.MkNumber_builder (Number_builder.Bin_builder)
module Oct_builder = Buffer_like.MkNumber_builder (Number_builder.Oct_builder)
module Hex_builder = Buffer_like.MkNumber_builder (Number_builder.Hex_builder)
module Dec_builder = Buffer_like.MkNumber_builder (Number_builder.Dec_builder)

type bin = Bin_builder.t
type oct = Oct_builder.t
type dec = Dec_builder.t
type hex = Hex_builder.t

type 'a radix_witness =
  | Bin : bin radix_witness
  | Oct : oct radix_witness
  | Dec : dec radix_witness
  | Hex : hex radix_witness

type 'a t = {
  inp_m : 'a Input.t;
  inp : 'a;
  case_sensitive_buf : case_sensitive Buffer.t;
  case_insensitive_buf : case_insensitive Buffer.t;
  bin_bldr : bin number_builder Buffer.t;
  oct_bldr : oct number_builder Buffer.t;
  dec_bldr : dec number_builder Buffer.t;
  hex_bldr : hex number_builder Buffer.t;
}

let next (type a) st =
  let module I = (val st.inp_m : Input.S with type t = a) in
  I.next st.inp

let peek (type a) st =
  let module I = (val st.inp_m : Input.S with type t = a) in
  I.peek st.inp

let skip (type a) st =
  let module I = (val st.inp_m : Input.S with type t = a) in
  I.skip st.inp

let with_case_insensitive_buf st f = Buffer.with_buf st.case_insensitive_buf f
let with_case_sensitive_buf st f = Buffer.with_buf st.case_sensitive_buf f
let continue_case_sensitive_buf st f = f st.case_sensitive_buf

let with_number_builder (type a) st (rdx : a radix_witness)
    (f : a number_builder Buffer.t -> 'b) =
  match rdx with
  | Bin -> Buffer.with_buf st.bin_bldr f
  | Oct -> Buffer.with_buf st.oct_bldr f
  | Dec -> Buffer.with_buf st.dec_bldr f
  | Hex -> Buffer.with_buf st.hex_bldr f

let add_to_buf_while_true st buf pred =
  let ch = ref (peek st) in
  while pred !ch do
    skip st;
    Buffer.add_char buf !ch;
    ch := peek st
  done

let consume_while_true st pred =
  let ch = ref (peek st) in
  while pred !ch do
    skip st;
    ch := peek st
  done

let consume_until_nl st =
  consume_while_true st (fun ch -> not @@ Char.is_newline ch)

let create inp_m inp =
  let open Buffer_like in
  let case_insensitive_buf = Lc_buffer.create 1024 in
  let case_sensitive_buf = Lc_buffer.to_case_sensitive case_insensitive_buf in
  {
    inp_m;
    inp;
    case_insensitive_buf =
      { buf_m = (module Lc_buffer); buf = case_insensitive_buf };
    case_sensitive_buf = { buf_m = (module Buffer); buf = case_sensitive_buf };
    bin_bldr =
      {
        buf_m = (module Bin_builder);
        buf =
          Bin_builder.create
            (Number_builder.Bin_builder.create ())
            case_sensitive_buf;
      };
    oct_bldr =
      {
        buf_m = (module Oct_builder);
        buf =
          Oct_builder.create
            (Number_builder.Oct_builder.create ())
            case_sensitive_buf;
      };
    hex_bldr =
      {
        buf_m = (module Hex_builder);
        buf =
          Hex_builder.create
            (Number_builder.Hex_builder.create ())
            case_sensitive_buf;
      };
    dec_bldr =
      {
        buf_m = (module Dec_builder);
        buf =
          Dec_builder.create
            (Number_builder.Dec_builder.create ())
            case_sensitive_buf;
      };
  }
