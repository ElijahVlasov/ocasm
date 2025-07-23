type 'a t

val create : 'a Input.t -> 'a -> 'a t
val next : 'a t -> char
val peek : 'a t -> char
val skip : 'a t -> unit

type bin
type oct
type dec
type hex

type 'a radix_witness =
  | Bin : bin radix_witness
  | Oct : oct radix_witness
  | Dec : dec radix_witness
  | Hex : hex radix_witness

type case_insensitive
type case_sensitive
type 'a number_builder

module Buffer : sig
  type 'a t

  val add_char : 'a t -> char -> unit
  val contents : 'a t -> string
  val lc_contents : case_insensitive t -> string
  val to_number : 'a number_builder t -> int64 array
  val clear : 'a t -> unit
end

val with_case_insensitive_buf : 'a t -> (case_insensitive Buffer.t -> 'b) -> 'b
val with_case_sensitive_buf : 'a t -> (case_sensitive Buffer.t -> 'b) -> 'b
val continue_case_sensitive_buf : 'a t -> (case_sensitive Buffer.t -> 'b) -> 'b

val with_number_builder :
  'a t -> 'b radix_witness -> ('b number_builder Buffer.t -> 'c) -> 'c

val add_to_buf_while_true : 'a t -> 'b Buffer.t -> (char -> bool) -> unit
val consume_while_true : 'a t -> (char -> bool) -> unit
val consume_until_nl : 'a t -> unit
