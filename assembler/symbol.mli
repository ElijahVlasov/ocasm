open Base

type 'a symbol = {
  section : Section.t;
  name : string;
  value : 'a;
  flags : Bfd.Symbol_flags.t;
}
