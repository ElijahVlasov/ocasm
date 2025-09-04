open! Import

type t

val create :
  ?fmt:Formatter.t ->
  ?filter:string Hash_set.t ->
  ?promote:string Hash_set.t ->
  unit ->
  t

val emit : t -> Diagnostics_message.t -> 'e -> (unit, 'e) Result.t
