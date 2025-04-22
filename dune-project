(lang dune 3.17)

(name ocasm)

(generate_opam_files true)

(using menhir 3.0)

(package
 (name ocasm)
 (version "0.0.1")
 (synopsis "RISC-V assembler in OCaml")
 (description "RISC-V assembler in OCaml")
 (maintainers "Ilia Vlasov <elijah.vlasov@gmail.com>")
 (authors "Ilia Vlasov <elijah.vlasov@gmail.com>")
 (license MIT)
 (depends ctypes ctypes-foreign ocaml ocamlformat ppx_deriving alcotest menhir getopt)
 (tags
  ("riscv" "assembler" "assembly"))
  (source
   (github ElijahVlasov/ocasm)))
