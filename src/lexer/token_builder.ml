open! Import

module Private = struct
  module type S = sig
    type t

    val add_char : t -> char -> unit
    val contents : t -> string
    val lc_contents : t -> string
    val to_number : t -> int64 array
    val clear : t -> unit
  end

  module Cs_builder : sig
    type t = Buffer.t

    include S with type t := t
    include module type of Buffer with type t := t
  end = struct
    include Buffer

    let to_number _ = Panic.unreachable ()
    let lc_contents _ = Panic.unreachable ()
    let to_case_sensitive _ = Panic.unreachable ()
  end

  module Ci_builder : sig
    include S

    val create : Buffer.t -> Buffer.t -> t
    val to_case_sensitive : t -> Buffer.t
  end = struct
    type t = { contents : Buffer.t; lc_contents : Buffer.t }

    let create cs ci = { contents = cs; lc_contents = ci }

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

type case_insensitive = Private.Ci_builder.t
type case_sensitive = Private.Cs_builder.t
type 'a number_builder = 'a
type 'a t = { buf_m : (module Private.S with type t = 'a); buf : 'a }

let add_char (type a) b ch =
  let module B = (val b.buf_m : Private.S with type t = a) in
  B.add_char b.buf ch

let contents (type a) b =
  let module B = (val b.buf_m : Private.S with type t = a) in
  B.contents b.buf

let lc_contents (b : case_insensitive t) =
  let module B = (val b.buf_m : Private.S with type t = case_insensitive) in
  B.lc_contents b.buf

let to_number (type a) (b : a number_builder t) =
  let module B = (val b.buf_m : Private.S with type t = a) in
  B.to_number b.buf

let clear (type a) b =
  let module B = (val b.buf_m : Private.S with type t = a) in
  B.clear b.buf

let with_builder buf f =
  clear buf;
  f buf

module Bin_builder = Private.MkNumber_builder (Number_builder.Bin_builder)
module Oct_builder = Private.MkNumber_builder (Number_builder.Oct_builder)
module Hex_builder = Private.MkNumber_builder (Number_builder.Hex_builder)
module Dec_builder = Private.MkNumber_builder (Number_builder.Dec_builder)

type bin = Bin_builder.t
type oct = Oct_builder.t
type dec = Dec_builder.t
type hex = Hex_builder.t

type 'a radix_witness =
  | Bin : bin radix_witness
  | Oct : oct radix_witness
  | Dec : dec radix_witness
  | Hex : hex radix_witness

let create_case_sensitive buf =
  let open Private in
  { buf_m = (module Cs_builder); buf }

let create_case_insensitive cs ci =
  let open Private in
  { buf_m = (module Ci_builder); buf = Ci_builder.create cs ci }

let create_number_builder : type a.
    Buffer.t -> a radix_witness -> a number_builder t =
 fun buf -> function
  | Bin ->
      {
        buf_m = (module Bin_builder);
        buf = Bin_builder.create (Number_builder.Bin_builder.create ()) buf;
      }
  | Oct ->
      {
        buf_m = (module Oct_builder);
        buf = Oct_builder.create (Number_builder.Oct_builder.create ()) buf;
      }
  | Dec ->
      {
        buf_m = (module Dec_builder);
        buf = Dec_builder.create (Number_builder.Dec_builder.create ()) buf;
      }
  | Hex ->
      {
        buf_m = (module Hex_builder);
        buf = Hex_builder.create (Number_builder.Hex_builder.create ()) buf;
      }
