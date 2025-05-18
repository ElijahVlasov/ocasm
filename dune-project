(lang dune 3.18)

(name ocasm)

(generate_opam_files true)

(using ctypes 0.3)
(using menhir 3.0)

(package
 (name ocasm)
 (version "0.0.1")
 (synopsis "RISC-V assembler in OCaml")
 (description "RISC-V assembler in OCaml")
 (maintainers "Ilia Vlasov <elijah.vlasov@gmail.com>")
 (authors "Ilia Vlasov <elijah.vlasov@gmail.com>")
 (license MIT)
 (depends
  alcotest
  ctypes
  ctypes-foreign
  getopt
  menhir
  (ocaml (>= 5.3.0))
  ocamlformat
  ppx_deriving
  ppx_deriving_hash
  ppx_sexp_conv
  )
 (tags
  ("riscv" "assembler" "assembly"))
  (source
   (github ElijahVlasov/ocasm)))
