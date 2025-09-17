open! Import

type t = { mutable commands : (unit, unit) Command.t Sequence.t }
