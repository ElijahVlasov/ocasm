open! Import

type nonrec 'a result =
  ('a, (Diagnostics.Error.t, Diagnostics.Warning.t) Either.t) Result.t

include struct
  open Token_builder

  type 'a t = {
    inp_m : 'a Positioned.t Input.t;
    inp : 'a Positioned.t;
    dgn_printer : Diagnostics_printer.t;
    case_sensitive_builder : case_sensitive Token_builder.t;
    case_insensitive_builder : case_insensitive Token_builder.t;
    bin_bldr : bin number_builder Token_builder.t;
    oct_bldr : oct number_builder Token_builder.t;
    dec_bldr : dec number_builder Token_builder.t;
    hex_bldr : hex number_builder Token_builder.t;
    mutable start_pos : Location.t;
  }
end

let next (type a) st =
  let module I = (val st.inp_m : Input.S with type t = a Positioned.t) in
  I.next st.inp

let peek (type a) st =
  let module I = (val st.inp_m : Input.S with type t = a Positioned.t) in
  I.peek st.inp

let skip (type a) st =
  let module I = (val st.inp_m : Input.S with type t = a Positioned.t) in
  I.skip st.inp

let path (type a) st =
  let module I = (val st.inp_m : Input.S with type t = a Positioned.t) in
  I.path st.inp

let line st = Positioned.line st.inp
let col st = Positioned.col st.inp
let pos st = Positioned.pos st.inp
let start_token st = st.start_pos <- pos st
let get_start st = st.start_pos

let with_case_insensitive_builder st f =
  Token_builder.with_builder st.case_insensitive_builder f

let with_case_sensitive_builder st f =
  Token_builder.with_builder st.case_sensitive_builder f

let continue_case_sensitive_builder st f = f st.case_sensitive_builder

let with_number_builder (type a) st rdx f =
  let open Token_builder in
  let builder : a number_builder Token_builder.t =
    match (rdx : a radix_witness) with
    | Bin -> st.bin_bldr
    | Oct -> st.oct_bldr
    | Dec -> st.dec_bldr
    | Hex -> st.hex_bldr
  in
  Token_builder.with_builder builder f

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

let emit_diagnostic_message st err_or_warn =
  let open Either in
  let open Diagnostics_message in
  let id, msg, ty =
    match err_or_warn with
    | First err ->
        ( Diagnostics.Error.id err,
          Diagnostics.Error.show err,
          Diagnostics_type.Error )
    | Second warn ->
        ( Diagnostics.Warning.id warn,
          Diagnostics.Warning.show warn,
          Diagnostics_type.Warning )
  in
  let pos = pos st in
  let dgn_msg =
    { id; ty; msg; starts = pos; ends = pos; file = path st; ctx = "" }
  in
  Diagnostics_printer.emit st.dgn_printer dgn_msg err_or_warn

let error st err =
  let open Result.Let_syntax in
  let err = Either.First err in
  (* We now that Diagnostics_printer will return Error anyway when we pass an
     error. *)
  let%bind () = emit_diagnostic_message st err in
  Error err

let warning st warn = emit_diagnostic_message st (Either.Second warn)

let create (type a) inp_m inp dgn_printer =
  let module I = (val inp_m : Input.S with type t = a) in
  let module I_pos = Input.MakePositioned (I) in
  let open Token_builder in
  let lower_case_buf = Buffer.create 1024 in
  let original_case_buf = Buffer.create 1024 in
  {
    inp_m = (module I_pos);
    inp = I_pos.create inp;
    dgn_printer;
    case_sensitive_builder = create_case_sensitive original_case_buf;
    case_insensitive_builder =
      create_case_insensitive original_case_buf lower_case_buf;
    bin_bldr = create_number_builder original_case_buf Bin;
    oct_bldr = create_number_builder original_case_buf Oct;
    dec_bldr = create_number_builder original_case_buf Dec;
    hex_bldr = create_number_builder original_case_buf Hex;
    start_pos = Location.default ();
  }
