open Base

let unimplemented () = failwith "Unimplemented"
let unreachable ?(msg = "Unreachable") () = failwith msg
let unreachable_when cond = if cond then failwith "Unreachable"
