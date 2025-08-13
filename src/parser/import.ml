include struct
  include Base
  include Ocasm_core
  include Ocasm_utils
  module Lexer = Ocasm_lexer
  module Isa_token = Lexer.Isa_token
  module Token = Lexer.Token
end
