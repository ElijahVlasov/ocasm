open Instruction
open Lexing
open Parser_error
module I = Parser.MenhirInterpreter

let lexbuf_to_invalid_syntax (lexbuf : Lexing.lexbuf) : parsing_error =
  InvalidSyntax (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let rec loop lexbuf
    (checkpoint : structured_instruction list parser_result I.checkpoint) :
    structured_instruction list parser_result =
  match checkpoint with
  | I.InputNeeded _env ->
      (* The parser needs a token. Request one from the lexer, and offer it to
         the parser, which will produce a new checkpoint. Then, repeat. *)
      let token = Lexer.token lexbuf in
      let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      loop lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      loop lexbuf checkpoint
  | I.HandlingError e ->
      (* The parser has suspended itself because of a syntax error. Stop. *)
      Error (ParsingError (lexbuf_to_invalid_syntax lexbuf))
  | I.Accepted v -> v
  | I.Rejected -> failwith "Cannot end up here"

let parse_program (program : string) : structured_instruction list parser_result
    =
  let lexbuf = Lexing.from_string program in
  try loop lexbuf (Parser.Incremental.text lexbuf.lex_curr_p)
  with Parser_error.LexError inner -> Error (LexingError inner)
