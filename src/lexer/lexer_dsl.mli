open! Import
open Token_builder

type 'a t

type nonrec 'a result =
  ('a, (Diagnostics.Error.t, Diagnostics.Warning.t) Either.t) Result.t

val create : 'a Input.t -> 'a -> Diagnostics_printer.t -> 'a t
val next : _ t -> char
val peek : _ t -> char
val skip : _ t -> unit
val pos : _ t -> Location.t
val line : _ t -> int
val col : _ t -> int
val start_token : _ t -> unit
val get_start : _ t -> Location.t
val path : _ t -> Path.t

val with_case_insensitive_builder :
  _ t -> (case_insensitive Token_builder.t -> 'b) -> 'b

val with_case_sensitive_builder :
  _ t -> (case_sensitive Token_builder.t -> 'b) -> 'b

val continue_case_sensitive_builder :
  _ t -> (case_sensitive Token_builder.t -> 'b) -> 'b

val with_number_builder :
  _ t -> 'b radix_witness -> ('b number_builder Token_builder.t -> 'c) -> 'c

val add_to_builder_while_true :
  _ t -> 'b Token_builder.t -> (char -> bool) -> unit

val consume_while_true : _ t -> (char -> bool) -> unit
val consume_until_nl : _ t -> unit
val warning : _ t -> Diagnostics.Warning.t -> unit result
val error : _ t -> Diagnostics.Error.t -> 'a result
