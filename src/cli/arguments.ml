type command = Null | Help | Assemble of string

let opts command = [ ('h', "help", Some (fun () -> command := Help), None) ]
let handle_filenames command filename = command := Assemble filename
