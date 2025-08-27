open! Import
include Relocatable_intf

let imm bs v = Imm (bs, v)
let name str = Name str
let reloc bit_size value reloc_data = Reloc { bit_size; value; reloc_data }

let of_big_integer value =
  let value = Big_integer.to_int64_exn value in
  let bit_size = 0 in
  imm bit_size value
