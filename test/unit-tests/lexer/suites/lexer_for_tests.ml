open! Import

let create_lexer (type a) inp_m inp =
  let open Mock_isa in
  let module Input = (val inp_m : Input.S with type t = a) in
  let module Lexer = Lexer.Mk (Mock_token) (Input) in
  let printer = Diagnostics_printer.create () in
  Lexer.create inp printer

let with_lexer content f =
  let module I = Lexer.Input.StringInput in
  let input = I.create content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer = create_lexer (module I) inp in
      f lexer)
