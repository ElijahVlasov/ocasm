open! Import
open Token_builder

type ('a, 'h) t

val create :
  'a Input.t ->
  'a ->
  (module Diagnostics_handler.S with type t = 'h) ->
  'h ->
  ('a, 'h) t

val next : ('a, 'h) t -> char
val peek : ('a, 'h) t -> char
val skip : ('a, 'h) t -> unit
val pos : ('a, 'h) t -> Location.t
val line : ('a, 'h) t -> int
val col : ('a, 'h) t -> int
val start_token : ('a, 'h) t -> unit
val get_start : ('a, 'h) t -> Location.t

val with_case_insensitive_builder :
  ('a, 'h) t -> (case_insensitive Token_builder.t -> 'b) -> 'b

val with_case_sensitive_builder :
  ('a, 'h) t -> (case_sensitive Token_builder.t -> 'b) -> 'b

val continue_case_sensitive_builder :
  ('a, 'h) t -> (case_sensitive Token_builder.t -> 'b) -> 'b

val with_number_builder :
  ('a, 'h) t ->
  'b radix_witness ->
  ('b number_builder Token_builder.t -> 'c) ->
  'c

val add_to_builder_while_true :
  ('a, 'h) t -> 'b Token_builder.t -> (char -> bool) -> unit

val consume_while_true : ('a, 'h) t -> (char -> bool) -> unit
val consume_until_nl : ('a, 'h) t -> unit
val warning : ('a, 'h) t -> ?k:Errors.recovery -> Errors.warning -> unit
val error : ('a, 'h) t -> ?k:Errors.recovery -> Errors.error -> 'b
val fail : ('a, 'h) t -> Errors.error -> 'b
