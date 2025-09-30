open! Import

type t = Nop | Mv of Pseudoregister.t * Pseudoregister.t
