open! Import

let token_info : Token_info.t testable =
  (module Token_info : TESTABLE with type t = Token_info.t)

let diagnostic_message : Diagnostic_message.Dyn.t testable =
  (module Diagnostic_message.Dyn : TESTABLE
    with type t = Diagnostic_message.Dyn.t)

let token (type a) (isa_token : a testable) : a Token.t testable =
  let module M = (val isa_token : TESTABLE with type t = a) in
  testable (Token.pp M.pp) (Token.equal M.equal)

let mock_token : Mock_token.t testable =
  (module Mock_token : TESTABLE with type t = Mock_token.t)
