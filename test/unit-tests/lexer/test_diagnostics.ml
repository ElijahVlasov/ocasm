open! Import

let () = List.iter Test_diagnostics.suite ~f:(fun x -> x ())
