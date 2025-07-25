open Base

type case_insensitive
type case_sensitive
type 'a number_builder
type bin
type oct
type dec
type hex

type 'a radix_witness =
  | Bin : bin radix_witness
  | Oct : oct radix_witness
  | Dec : dec radix_witness
  | Hex : hex radix_witness

type 'a t

val add_char : 'a t -> char -> unit
val contents : 'a t -> string
val lc_contents : case_insensitive t -> string
val to_number : 'a number_builder t -> int64 array
val with_builder : 'a t -> ('a t -> 'b) -> 'b
val clear : 'a t -> unit
val create_case_sensitive : Buffer.t -> case_sensitive t
val create_case_insensitive : Buffer.t -> Buffer.t -> case_insensitive t
val create_number_builder : Buffer.t -> 'a radix_witness -> 'a number_builder t
