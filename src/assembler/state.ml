type 'a t = {
  symtab_state : 'a Symtab_state.t;
  section_state : Section_state.t;
}
[@@deriving fields]

let create wt ~secs =
  {
    symtab_state = Symtab_state.create wt;
    section_state = Section_state.create secs;
  }

let finalize s =
  (Symtab_state.finalize s.symtab_state, Section_state.finalize s.section_state)
