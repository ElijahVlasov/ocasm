open! Import

type ('reg, 'dir, 'opcode, 'res, 'rel, 'out) t = {
  path : Path.t;
  opcode_builder : ('reg, 'opcode, 'rel, 'out) Builder.t;
  dir_builder : ('reg, 'dir, 'rel, 'out) Builder.t;
  res_builder : ('reg, 'res, 'rel, 'out) Builder.t;
  token_reader : ('reg, 'dir, 'opcode, 'res) Isa.Token.t Token_reader.t;
}
[@@deriving fields]

let with_opcode_builder st opcode f =
  Builder.start st.opcode_builder opcode;
  f st.opcode_builder

let with_dir_builder st dir f =
  Builder.start st.dir_builder dir;
  f st.dir_builder

let add_register _st bldr reg = Builder.add_register bldr reg
let add_rel _st bldr rel = Builder.add_rel bldr rel
let add_string _st bldr str = Builder.add_string bldr str
let add_base_offset _st bldr base off = Builder.add_base_offset bldr base off
let build _st bldr = Builder.build bldr
let next st = Token_reader.next st.token_reader
let peek st = Token_reader.peek st.token_reader

let skip st =
  let _ = Token_reader.next st.token_reader in
  ()

let last_token_info st = Token_reader.last_token_info st.token_reader

let rec next_non_whitespace st =
  let open Token in
  match next st with White_space -> next_non_whitespace st | tok -> tok

let rec peek_non_whitespace st =
  let open Token in
  match peek st with
  | White_space ->
      skip st;
      peek_non_whitespace st
  | tok -> tok

let skip_whitespaces_and_newlines st =
  let tok = ref (next_non_whitespace st) in
  while Token.is_eol !tok do
    tok := next_non_whitespace st
  done

let create ?(path = Path.empty) reg_m dir_m opcode_m res_m ~word_size
    ~build_instruction ~build_directive ~build_reserved toks =
  {
    path;
    opcode_builder =
      Builder.create reg_m opcode_m ~word_size ~builder_fn:build_instruction;
    dir_builder =
      Builder.create reg_m dir_m ~word_size ~builder_fn:build_directive;
    res_builder =
      Builder.create reg_m res_m ~word_size ~builder_fn:build_reserved;
    token_reader = Token_reader.create toks;
  }
