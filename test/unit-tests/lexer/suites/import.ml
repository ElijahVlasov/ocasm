include struct
  include Alcotest
  include Base
  include Ocore
  include Lexer
  include Test_common
  include Utils
  module Int64 = Stdlib.Int64
  module Lexer = Lexer.Mk (Mock_isa.Mock_token)
end
