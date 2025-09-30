open! Import

type reg_or_pseudoreg = (Register.t, Pseudoregister.t) Either.t
type t = Nop | Mv of reg_or_pseudoreg * reg_or_pseudoreg
