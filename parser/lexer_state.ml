open Base
open Token_builder
open Ocasm_utils

type 'a t = {
  inp_m : 'a Input.t;
  inp : 'a;
  case_sensitive_builder : case_sensitive Token_builder.t;
  case_insensitive_builder : case_insensitive Token_builder.t;
  bin_bldr : bin number_builder Token_builder.t;
  oct_bldr : oct number_builder Token_builder.t;
  dec_bldr : dec number_builder Token_builder.t;
  hex_bldr : hex number_builder Token_builder.t;
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

let with_case_insensitive_builder st f =
  Token_builder.with_builder st.case_insensitive_builder f

let with_case_sensitive_builder st f =
  Token_builder.with_builder st.case_sensitive_builder f

let continue_case_sensitive_builder st f = f st.case_sensitive_builder

let with_number_builder (type a) st (rdx : a radix_witness)
    (f : a number_builder Token_builder.t -> 'b) =
  match rdx with
  | Bin -> Token_builder.with_builder st.bin_bldr f
  | Oct -> Token_builder.with_builder st.oct_bldr f
  | Dec -> Token_builder.with_builder st.dec_bldr f
  | Hex -> Token_builder.with_builder st.hex_bldr f

let add_to_builder_while_true st builder pred =
  let ch = ref (peek st) in
  while pred !ch do
    skip st;
    Token_builder.add_char builder !ch;
    ch := peek st
  done

let consume_while_true st pred =
  let ch = ref (peek st) in
  while pred !ch do
    skip st;
    ch := peek st
  done

let consume_until_nl st =
  consume_while_true st (fun ch ->
      not @@ (Char.is_newline ch || Char.is_eof ch))

let create inp_m inp =
  let open Token_builder in
  let lower_case_buf = Buffer.create 1024 in
  let original_case_buf = Buffer.create 1024 in
  {
    inp_m;
    inp;
    case_sensitive_builder = create_case_sensitive original_case_buf;
    case_insensitive_builder =
      create_case_insensitive original_case_buf lower_case_buf;
    bin_bldr = create_number_builder original_case_buf Bin;
    oct_bldr = create_number_builder original_case_buf Oct;
    dec_bldr = create_number_builder original_case_buf Dec;
    hex_bldr = create_number_builder original_case_buf Hex;
  }
