open Getopt

type command = Null | Help | Assemble of string

val opts : command ref -> opt list
val handle_filenames : command ref -> string -> unit
