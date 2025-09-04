open! Import

let create_lexer inp_m inp =
  let open Mock_isa in
  let printer = Diagnostics_printer.create () in
  Lexer.create (module Mock_token) inp_m inp printer

let with_lexer content f =
  let module I = Lexer.Input.StringInput in
  let input = I.create content in
  Input.with_input
    (module I)
    input
    ~f:(fun inp ->
      let lexer = create_lexer (module I) inp in
      f lexer)
