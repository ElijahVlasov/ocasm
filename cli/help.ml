let show_help () =
  let usage = "Usage: ocasm [options] <filename>" in
  let help = "RISC-V assembler" in
  let options = "Options:" in
  let options_list = "  -h, --help  Display this help message" in
  Printf.printf "%s\n%s\n%s\n%s\n" usage help options options_list
