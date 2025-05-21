open Bfd.CArray
open Writer

let () =
  write_object_file ~word_type:Word32 ~name:"test_w"
    ~sections:
      [ (Data, [ 0x69l; 0x02l; 0x03l ]); (Text, [ 0x72l; 0x22l; 0x33l ]) ]
    ~symtab:
      [
        {
          section = Text;
          name = "_start";
          value = 0x69l;
          flags = Bfd.Symbol_flags.bsf_global;
        };
      ]
