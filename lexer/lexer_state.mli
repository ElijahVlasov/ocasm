open Base
open Token_builder

type 'a t

val create : 'a Input.t -> 'a -> 'a t
val next : 'a t -> char
val peek : 'a t -> char
val skip : 'a t -> unit
val pos : 'a t -> int * int
val line : 'a t -> int
val col : 'a t -> int
val start_token : 'a t -> unit
val get_start : 'a t -> int * int

val with_case_insensitive_builder :
  'a t -> (case_insensitive Token_builder.t -> 'b) -> 'b

val with_case_sensitive_builder :
  'a t -> (case_sensitive Token_builder.t -> 'b) -> 'b

val continue_case_sensitive_builder :
  'a t -> (case_sensitive Token_builder.t -> 'b) -> 'b

val with_number_builder :
  'a t -> 'b radix_witness -> ('b number_builder Token_builder.t -> 'c) -> 'c

val add_to_builder_while_true :
  'a t -> 'b Token_builder.t -> (char -> bool) -> unit

val consume_while_true : 'a t -> (char -> bool) -> unit
val consume_until_nl : 'a t -> unit
