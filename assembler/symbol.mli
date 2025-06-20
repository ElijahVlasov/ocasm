open Base
open Bfd

type 'a t = {
  section : Section.t;
  name : string;
  value : 'a;
  flags : Symbol_flags.t;
}
