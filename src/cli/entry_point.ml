open Arguments
open Assemble_file
open Getopt
open Help

let entry_point () =
  let command = ref Null in
  let opts = parse_cmdline (opts command) (handle_filenames command) in
  match !command with
  | Help -> show_help ()
  | Assemble filename -> assemble_file filename
  | _ -> show_help ()
