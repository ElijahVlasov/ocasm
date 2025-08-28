include struct
  include Base
  include Ocasm_core
  include Ocasm_utils
  include Lexer
  module Isa_token = Lexer.Isa_token
  module Token = Lexer.Token
  module Token_info = Lexer.Token_info
end
