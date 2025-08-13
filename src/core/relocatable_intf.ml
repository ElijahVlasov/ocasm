type 'a t =
  | Imm of int * int64
  | Name of string
  | Reloc of { bit_size : int; value : int64; reloc_data : 'a }
[@@deriving eq, show]
