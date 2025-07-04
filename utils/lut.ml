open Base

type t = char Hash_set.t

let create cs = Hash_set.of_list (module Char) cs
let mem = Hash_set.mem
