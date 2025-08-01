%{
open Immediate
open Instruction
open Register
open Parser_error

let ( let+ ) a b = Result.bind a b

let ( and+ ) a b =
  let+ a' = a in
  let+ b' = b in
  Ok (a', b')

let emptytext = Ok []

let mktext instr rst =
  let+ instr = instr and+ rst = rst in
  Ok (instr :: rst)

let mkregister name : register parser_result =
  Option.to_result ~none:(ParsingError (InvalidRegister name))
    (Register.of_string name)

let mkaddi rd rs1 imm =
  let+ rd = rd
  and+ rs1 = rs1
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Add (rd, rs1, Imm imm))

let mkadd rd rs1 rs2 =
  let+ rd = rd and+ rs1 = rs1 and+ rs2 = rs2 in
  Ok (Add (rd, rs1, Reg rs2))

let mksub rd rs1 rs2 =
  let+ rd = rd and+ rs1 = rs1 and+ rs2 = rs2 in
  Ok (Sub (rd, rs1, rs2))

let mkand rd rs1 rs2 =
  let+ rd = rd and+ rs1 = rs1 and+ rs2 = rs2 in
  Ok (And (rd, rs1, Reg rs2))

let mkandi rd rs1 imm =
  let+ rd = rd
  and+ rs1 = rs1
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (And (rd, rs1, Imm imm))

let mkor rd rs1 rs2 =
  let+ rd = rd and+ rs1 = rs1 and+ rs2 = rs2 in
  Ok (Or (rd, rs1, Reg rs2))

let mkori rd rs1 imm =
  let+ rd = rd
  and+ rs1 = rs1
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Or (rd, rs1, Imm imm))

let mkxor rd rs1 rs2 =
  let+ rd = rd and+ rs1 = rs1 and+ rs2 = rs2 in
  Ok (Xor (rd, rs1, Reg rs2))

let mkxori rd rs1 imm =
  let+ rd = rd
  and+ rs1 = rs1
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Xor (rd, rs1, Imm imm))

let mkslt rd rs1 rs2 =
  let+ rd = rd and+ rs1 = rs1 and+ rs2 = rs2 in
  Ok (Slt (rd, rs1, Reg rs2))

let mkslti rd rs1 imm =
  let+ rd = rd
  and+ rs1 = rs1
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Slt (rd, rs1, Imm imm))

let mksltu rd rs1 rs2 =
  let+ rd = rd and+ rs1 = rs1 and+ rs2 = rs2 in
  Ok (Sltu (rd, rs1, Reg rs2))

let mksltui rd rs1 imm =
  let+ rd = rd
  and+ rs1 = rs1
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Sltu (rd, rs1, Imm imm))

let mksll rd rs1 rs2 =
  let+ rd = rd and+ rs1 = rs1 and+ rs2 = rs2 in
  Ok (Sll (rd, rs1, Reg rs2))

let mkslli rd rs1 imm =
  let+ rd = rd
  and+ rs1 = rs1
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Sll (rd, rs1, Imm imm))

let mksrl rd rs1 rs2 =
  let+ rd = rd and+ rs1 = rs1 and+ rs2 = rs2 in
  Ok (Srl (rd, rs1, Reg rs2))

let mksrli rd rs1 imm =
  let+ rd = rd
  and+ rs1 = rs1
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Srl (rd, rs1, Imm imm))

let mksra rd rs1 rs2 =
  let+ rd = rd and+ rs1 = rs1 and+ rs2 = rs2 in
  Ok (Sra (rd, rs1, Reg rs2))

let mksrai rd rs1 imm =
  let+ rd = rd
  and+ rs1 = rs1
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Sra (rd, rs1, Imm imm))

let mklb rd rs2 imm =
  let+ rd = rd
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Lb (rd, rs2, imm))

let mklh rd rs2 imm =
  let+ rd = rd
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Lh (rd, rs2, imm))

let mklw rd rs2 imm =
  let+ rd = rd
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Lw (rd, rs2, imm))

let mklbu rd rs2 imm =
  let+ rd = rd
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Lbu (rd, rs2, imm))

let mklhu rd rs2 imm =
  let+ rd = rd
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Lhu (rd, rs2, imm))

let mksb rd rs2 imm =
  let+ rd = rd
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Sb (rd, rs2, imm))

let mksh rd rs2 imm =
  let+ rd = rd
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Sh (rd, rs2, imm))

let mksw rd rs2 imm =
  let+ rd = rd
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Sw (rd, rs2, imm))

let mkbeq rs1 rs2 imm =
  let+ rs1 = rs1
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Beq (rs1, rs2, imm))

let mkbne rs1 rs2 imm =
  let+ rs1 = rs1
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Bne (rs1, rs2, imm))

let mkblt rs1 rs2 imm =
  let+ rs1 = rs1
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Blt (rs1, rs2, imm))

let mkbge rs1 rs2 imm =
  let+ rs1 = rs1
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Bge (rs1, rs2, imm))

let mkbltu rs1 rs2 imm =
  let+ rs1 = rs1
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Bltu (rs1, rs2, imm))

let mkbgeu rs1 rs2 imm =
  let+ rs1 = rs1
  and+ rs2 = rs2
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Bgeu (rs1, rs2, imm))

let mkjal rd imm =
  let+ rd = rd
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate20.of_int32 imm)
  in
  Ok (Jal (rd, imm))

let mkjalr rd rs1 imm =
  let+ rd = rd
  and+ rs1 = rs1
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate12.of_int32 imm)
  in
  Ok (Jalr (rd, rs1, imm))

let mklui rd imm =
  let+ rd = rd
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate20.of_int32 imm)
  in
  Ok (Lui (rd, imm))

let mkauipc rd imm =
  let+ rd = rd
  and+ imm =
    Option.to_result ~none:(ParsingError (InvalidImmediate imm))
      (Immediate20.of_int32 imm)
  in
  Ok (Auipc (rd, imm))
%}

%token COMMA DOUBLE_QUOTES
%token <int32> NUM
%token PLUS MINUS TIMES DIVIDE
%token ADD SUB AND OR XOR SLT SLTU SLL SRL SRA
%token LB LH LW LBU LHU SB SH SW
%token BEQ BNE BLT BGE BLTU BGEU
%token JAL JALR LUI AUIPC
%token ECALL EBREAK FENCE FENCE_I FENCE_T FENCE_TS
%token ASCII BYTE
%token <string> STRING
%token <string> REG
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIVIDE

%start <structured_instruction list parser_result> text
%type <Directives.t> directive
%type <string list> string_list
%type <register parser_result> reg
%type <structured_instruction parser_result> instruction

%%

text:
  | EOF { emptytext }
  | i = instruction; p = text { mktext i p }

string_list:
  | STRING { [$1] }
  | s = STRING COMMA tail = string_list { s :: tail }

directive:
  | ASCII string_list { Directives.Ascii $1 }

reg:
  | REG { mkregister $1 }

instruction:
  | ADD rds = reg COMMA rs1 = reg COMMA imm = NUM { mkaddi rds rs1 imm }
  | ADD rds = reg COMMA rs1 = reg COMMA rs2 = reg { mkadd rds rs1 rs2 } 
  | SUB rds = reg COMMA rs1 = reg COMMA rs2 = reg { mksub rds rs1 rs2 }
  | AND rds = reg COMMA rs1 = reg COMMA rs2 = reg { mkand rds rs1 rs2 }
  | AND rds = reg COMMA rs1 = reg COMMA imm = NUM { mkandi rds rs1 imm }
  | OR rds = reg COMMA rs1 = reg COMMA rs2 = reg { mkor rds rs1 rs2 }
  | OR rds = reg COMMA rs1 = reg COMMA imm = NUM { mkori rds rs1 imm }
  | XOR rds = reg COMMA rs1 = reg COMMA rs2 = reg { mkxor rds rs1 rs2 }
  | XOR rds = reg COMMA rs1 = reg COMMA imm = NUM { mkxori rds rs1 imm }
  | SLT rds = reg COMMA rs1 = reg COMMA rs2 = reg { mkslt rds rs1 rs2 }
  | SLT rds = reg COMMA rs1 = reg COMMA imm = NUM { mkslti rds rs1 imm }
  | SLTU rds = reg COMMA rs1 = reg COMMA rs2 = reg { mksltu rds rs1 rs2 }
  | SLTU rds = reg COMMA rs1 = reg COMMA imm = NUM { mksltui rds rs1 imm }
  | SLL rds = reg COMMA rs1 = reg COMMA rs2 = reg { mksll rds rs1 rs2 }
  | SLL rds = reg COMMA rs1 = reg COMMA imm = NUM { mkslli rds rs1 imm }
  | SRL rds = reg COMMA rs1 = reg COMMA rs2 = reg { mksrl rds rs1 rs2 }
  | SRL rds = reg COMMA rs1 = reg COMMA imm = NUM { mksrli rds rs1 imm }
  | SRA rds = reg COMMA rs1 = reg COMMA rs2 = reg { mksra rds rs1 rs2 }
  | SRA rds = reg COMMA rs1 = reg COMMA imm = NUM { mksrai rds rs1 imm }
  | LB rds = reg COMMA rs2 = reg COMMA imm = NUM { mklb rds rs2 imm }
  | LH rds = reg COMMA rs2 = reg COMMA imm = NUM { mklh rds rs2 imm }
  | LW rds = reg COMMA rs2 = reg COMMA imm = NUM { mklw rds rs2 imm }
  | LBU rds = reg COMMA rs2 = reg COMMA imm = NUM { mklbu rds rs2 imm }
  | LHU rds = reg COMMA rs2 = reg COMMA imm = NUM { mklhu rds rs2 imm }
  | SB rds = reg COMMA rs2 = reg COMMA imm = NUM { mksb rds rs2 imm }
  | SH rds = reg COMMA rs2 = reg COMMA imm = NUM { mksh rds rs2 imm }
  | SW rds = reg COMMA rs2 = reg COMMA imm = NUM { mksw rds rs2 imm }
  | BEQ rs1 = reg COMMA rs2 = reg COMMA imm = NUM { mkbeq rs1 rs2 imm }
  | BNE rs1 = reg COMMA rs2 = reg COMMA imm = NUM { mkbne rs1 rs2 imm }
  | BLT rs1 = reg COMMA rs2 = reg COMMA imm = NUM { mkblt rs1 rs2 imm }
  | BGE rs1 = reg COMMA rs2 = reg COMMA imm = NUM { mkbge rs1 rs2 imm }
  | BLTU rs1 = reg COMMA rs2 = reg COMMA imm = NUM { mkbltu rs1 rs2 imm }
  | BGEU rs1 = reg COMMA rs2 = reg COMMA imm = NUM { mkbgeu rs1 rs2 imm }
  | JAL rd = reg COMMA imm = NUM { mkjal rd imm }
  | JALR rd = reg COMMA rs1 = reg COMMA imm = NUM { mkjalr rd rs1 imm }
  | LUI rd = reg COMMA imm = NUM { mklui rd imm }
  | AUIPC rd = reg COMMA imm = NUM { mkauipc rd imm }
  | ECALL { Ok Ecall }
  | EBREAK { Ok Ebreak }
  (* TODO: Add fence *)
