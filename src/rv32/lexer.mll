{
  open Parser
  open Parser_error
}

let digit = ['0'-'9']
let whitespace = [' ' '\t' '\r' '\n']
let separator = whitespace | eof | [',' ';']
let non_separator = [^' ' '\t' '\r' '\n' ',' ';']
let comma = ','
let double_quotes = '"'

rule token = parse
  | whitespace { token lexbuf }  (* Skip whitespace *)
  | ',' { COMMA }
  | digit+ as num { NUM (Int32.of_string num) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIVIDE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "add" { ADD }
  | "sub" { SUB }
  | "and" { AND }
  | "or" { OR }
  | "xor" { XOR }
  | "slt" { SLT }
  | "sltu" { SLTU }
  | "sll" { SLL }
  | "srl" { SRL }
  | "sra" { SRA }
  | "lb" { LB }
  | "lh" { LH }
  | "lw" { LW }
  | "lbu" { LBU }
  | "lhu" { LHU }
  | "sb" { SB }
  | "sh" { SH }
  | "sw" { SW }
  | "beq" { BEQ }
  | "bne" { BNE }
  | "blt" { BLT }
  | "bge" { BGE }
  | "bltu" { BLTU }
  | "bgeu" { BGEU }
  | "jal" { JAL }
  | "jalr" { JALR }
  | "lui" { LUI }
  | "auipc" { AUIPC }
  | "ecall" { ECALL }
  | "ebreak" { EBREAK }
  | "fence" { FENCE }
  | "fence.i" { FENCE_I }
  | "fence.t" { FENCE_T }
  | "fence.ts" { FENCE_TS }
  (* Directives: *)
  | ".ascii" { ASCII } 
  | ".byte" { BYTE }
  | (['0'-'9' 'a'-'z' 'A'-'Z']+ as name) { REG name }
  | "/*" { multiline_comment lexbuf }
  | '#' { single_line_comment lexbuf }
  | double_quotes { read_string (Buffer.create 256) lexbuf }
  | eof { EOF }
  | _ { raise (Parser_error.LexError (InvalidCharacter (Lexing.lexeme lexbuf))) }

and multiline_comment = parse
  | "*/" { token lexbuf }
  | _ { multiline_comment lexbuf }

and single_line_comment = parse
  | '\n' { token lexbuf }
  | _ { single_line_comment lexbuf }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
  | eof { failwith "String is not terminated" }
