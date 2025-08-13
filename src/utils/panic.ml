open Base

let unimplemented () = failwith "Unimplemented"
let unreachable () = failwith "Unreachable"
let unreachable_when cond = if cond then failwith "Unreachable"
