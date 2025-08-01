open Rv32.Parser_frontend
open Rv32.Instruction
open Writer

let assemble program =
  let structured_instructions = parse_program program in
  match structured_instructions with
  | Ok structured_instructions ->
      let raw_instructions =
        List.map
          (fun structured_instruction ->
            RawInstruction.to_int (ast_to_raw structured_instruction))
          structured_instructions
      in
      write_object_file ~wt:Word32 ~file_name:"a.out"
        ~sections:[ (Text, raw_instructions) ]
        ~symtab:[]
  | Error error -> failwith "Failed to parse instruction"

let assemble_file filename =
  let ic = open_in filename in
  let program = really_input_string ic (in_channel_length ic) in
  let () = close_in ic in
  assemble program
