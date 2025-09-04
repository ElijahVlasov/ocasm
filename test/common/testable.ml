open! Import

let token_info : Token_info.t testable =
  (module Token_info : TESTABLE with type t = Token_info.t)

let token (type a) (isa_token : a testable) : a Token.t testable =
  let module M = (val isa_token : TESTABLE with type t = a) in
  testable (Token.pp M.pp) (Token.equal M.equal)

let mock_token : Mock_isa.Mock_token.t testable =
  (module Mock_isa.Mock_token : TESTABLE with type t = Mock_isa.Mock_token.t)

let mock_instruction : Mock_isa.Mock_instruction.t testable =
  (module Mock_isa.Mock_instruction : TESTABLE
    with type t = Mock_isa.Mock_instruction.t)

let mock_directive : Mock_isa.Mock_directive.t testable =
  (module Mock_isa.Mock_directive : TESTABLE
    with type t = Mock_isa.Mock_directive.t)

let command (type instr) (type dir) (instr : instr testable)
    (dir : dir testable) : (instr, dir) Command.t testable =
  let module I = (val instr : TESTABLE with type t = instr) in
  let module D = (val dir : TESTABLE with type t = dir) in
  testable (Command.pp I.pp D.pp) (Command.equal I.equal D.equal)
