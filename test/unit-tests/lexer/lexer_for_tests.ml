open! Import

let create_lexer_and_handler inp_m inp =
  let open Mock_isa in
  let open Diagnostics_handler in
  let handler = Kitchen_sink_handler.create () in
  let lexer =
    Lexer.create
      (module Mock_token)
      inp_m inp
      (module Kitchen_sink_handler)
      handler
  in
  (lexer, handler)

let with_lexer content f =
  let module I = Lexer.Input.StringInput in
  let input = I.create content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer, _ = create_lexer_and_handler (module I) inp in
      f lexer)

let with_lexer_and_handler content f =
  let module I = Lexer.Input.StringInput in
  let input = I.create content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer, handler = create_lexer_and_handler (module I) inp in
      f lexer handler)
