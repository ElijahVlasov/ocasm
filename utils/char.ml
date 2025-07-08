include Base.Char

let is_binary = function '0' | '1' -> true | _ -> false
let is_octal = function '0' .. '7' -> true | _ -> false
let is_newline = function '\n' -> true | _ -> false
