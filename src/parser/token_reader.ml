open! Import

type token_info = Lexer.Token_info.t

type 't t = {
  mutable toks : ('t Token.t * token_info) option Sequence.t;
  mutable is_rolled_back : bool;
  mutable cur_tok_and_info : ('t Token.t * token_info) option;
}

let next st =
  let open Token in
  let open Token_info in
  if st.is_rolled_back then (
    st.is_rolled_back <- false;
    st.cur_tok_and_info)
  else
    let tok_and_tok_info =
      match Sequence.next st.toks with
      | None ->
          Some
            ( Eof,
              (* TODO: I wish we had Rust's Default trait. :) *)
              {
                starts = Location.create 1 1;
                ends = Location.create 1 1;
                string = (fun () -> "");
              } )
      | Some (next, tail) ->
          st.toks <- tail;
          next
    in
    st.cur_tok_and_info <- tok_and_tok_info;
    tok_and_tok_info

let roll_back st =
  if st.is_rolled_back then failwith "Rolling back too many times";
  st.is_rolled_back <- true

let create toks = { toks; is_rolled_back = false; cur_tok_and_info = None }
