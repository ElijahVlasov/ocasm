open! Import

let suite = [ ("Test snippets", Test_snippets.suite) ]
let () = Alcotest.run "Parser" suite
