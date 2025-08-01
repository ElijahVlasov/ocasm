open Base
include Array

let fill_from_list arr lst = List.iteri lst ~f:(Array.set arr)
