open Import

include struct
  open Token_builder

  type ('a, 'h) t = {
    inp_m : 'a Positioned.t Input.t;
    pos_m : (module Positioned.S0 with type t = 'a Positioned.t);
    inp : 'a Positioned.t;
    dgn_handler_m : (module Diagnostics_handler.S with type t = 'h);
    dgn_handler : 'h;
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

let line (type a) st =
  let module P = (val st.pos_m : Positioned.S0 with type t = a Positioned.t) in
  P.line st.inp

let col (type a) st =
  let module P = (val st.pos_m : Positioned.S0 with type t = a Positioned.t) in
  P.col st.inp

let pos (type a) st =
  let module P = (val st.pos_m : Positioned.S0 with type t = a Positioned.t) in
  let line, col = P.pos st.inp in
  Location.create line col

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

let diagnostics_aux (type h) st ?k msg =
  let recovery_to_func st =
    let open Errors in
    function
    | No_recovery -> None
    | Read_to_eol -> Some (fun () -> consume_until_nl st)
    | Custom k -> Some k
  in
  let module Diagnostics_handler =
    (val st.dgn_handler_m : Diagnostics_handler.S with type t = h)
  in
  let open Errors in
  let recovery = Option.value ~default:Read_to_eol k |> recovery_to_func st in
  Diagnostics_handler.throw ?recovery st.dgn_handler @@ msg

let warning st ?k warn =
  let open Errors in
  let open Warning in
  Warning.to_diagnostic_message (Warning warn) (pos st) (pos st)
    (Path.of_string "") ""
  |> diagnostics_aux ?k st

let error st ?k err =
  let open Errors in
  let open Error in
  Error.to_diagnostic_message (Error err) (pos st) (pos st) (Path.of_string "")
    ""
  |> diagnostics_aux ?k st

let fail st err =
  let open Errors in
  error st ~k:No_recovery err

let create (type a) inp_m inp dgn_handler_m dgn_handler =
  let module I = (val inp_m : Input.S with type t = a) in
  let module I_pos = Input.MakePositioned (I) in
  let open Token_builder in
  let lower_case_buf = Buffer.create 1024 in
  let original_case_buf = Buffer.create 1024 in
  {
    inp_m = (module I_pos);
    pos_m = (module I_pos);
    inp = I_pos.create inp;
    dgn_handler_m;
    dgn_handler;
    case_sensitive_builder = create_case_sensitive original_case_buf;
    case_insensitive_builder =
      create_case_insensitive original_case_buf lower_case_buf;
    bin_bldr = create_number_builder original_case_buf Bin;
    oct_bldr = create_number_builder original_case_buf Oct;
    dec_bldr = create_number_builder original_case_buf Dec;
    hex_bldr = create_number_builder original_case_buf Hex;
    start_pos = Location.create 1 1;
  }
