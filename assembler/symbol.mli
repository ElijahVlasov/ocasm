open Base
open Ocasm_binary

type 'a t = {
  section : Section.t;
  name : string;
  value : 'a;
  flags : Symbol_flags.t;
}
