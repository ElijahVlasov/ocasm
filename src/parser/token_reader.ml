open! Import

type token_info = Lexer.Token_info.t

exception Lexer_error

type 't t = {
  mutable toks : ('t Token.t * token_info) option Sequence.t;
  mutable peeked : ('t Token.t * token_info) option;
  mutable cur_tok_info : token_info option;
}

let fetch_next st =
  let open Token in
  match Sequence.next st.toks with
  | None -> (Eof, Token_info.default ())
  | Some (None, _) -> raise Lexer_error
  | Some (Some next, tail) ->
      st.toks <- tail;
      next

let next st =
  match st.peeked with
  | None ->
      let next, info = fetch_next st in
      st.cur_tok_info <- Some info;
      next
  | Some (next, info) ->
      st.cur_tok_info <- Some info;
      next

let peek st =
  match st.peeked with
  | None ->
      let next = fetch_next st in
      st.peeked <- Some next;
      fst next
  | Some (next, info) -> next

let last_token_info st =
  Option.value_or_thunk
    ~default:(fun () ->
      Panic.unreachable
        ~msg:"Cannot request the last token info if there's not been any reads"
        ())
    st.cur_tok_info

let create toks = { toks; peeked = None; cur_tok_info = None }
