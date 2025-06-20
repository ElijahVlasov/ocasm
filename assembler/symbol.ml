open Base

type 'a t = {
  section : Section.t;
  name : string;
  value : 'a;
  flags : Ocasm_binary.Symbol_flags.t;
}
