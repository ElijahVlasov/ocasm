open Base

type t = ROData | Text | Data | Bss [@@deriving eq, ord, hash, compare, sexp]

let to_string = function
  | ROData -> ".rodata"
  | Data -> ".data"
  | Text -> ".text"
  | Bss -> ".bss"
