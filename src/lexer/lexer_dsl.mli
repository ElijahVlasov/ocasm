open! Import
open Token_builder

type ('a, 'h) t

val create :
  'a Input.t ->
  'a ->
  (module Diagnostics_handler.S with type t = 'h) ->
  'h ->
  ('a, 'h) t

val next : (_, _) t -> char
val peek : (_, _) t -> char
val skip : (_, _) t -> unit
val pos : (_, _) t -> Location.t
val line : (_, _) t -> int
val col : (_, _) t -> int
val start_token : (_, _) t -> unit
val get_start : (_, _) t -> Location.t
val path : (_, _) t -> Path.t

val with_case_insensitive_builder :
  (_, _) t -> (case_insensitive Token_builder.t -> 'b) -> 'b

val with_case_sensitive_builder :
  (_, _) t -> (case_sensitive Token_builder.t -> 'b) -> 'b

val continue_case_sensitive_builder :
  (_, _) t -> (case_sensitive Token_builder.t -> 'b) -> 'b

val with_number_builder :
  (_, _) t ->
  'b radix_witness ->
  ('b number_builder Token_builder.t -> 'c) ->
  'c

val add_to_builder_while_true :
  (_, _) t -> 'b Token_builder.t -> (char -> bool) -> unit

val consume_while_true : (_, _) t -> (char -> bool) -> unit
val consume_until_nl : (_, _) t -> unit
val warning : (_, _) t -> ?k:Errors.recovery -> Errors.warning -> unit
val error : (_, _) t -> ?k:Errors.recovery -> Errors.error -> 'b
val fail : (_, _) t -> Errors.error -> 'b
