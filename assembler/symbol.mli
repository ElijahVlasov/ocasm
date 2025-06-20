open Base

type 'a t = {
  section : Section.t;
  name : string;
  value : 'a;
  flags : Bfd.Symbol_flags.t;
}
