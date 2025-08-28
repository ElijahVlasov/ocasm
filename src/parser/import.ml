include struct
  include Base
  include Ocore
  include Utils
  include Lexer
  module Isa_token = Lexer.Isa_token
  module Token = Lexer.Token
  module Token_info = Lexer.Token_info
end
