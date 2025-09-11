open! Import

type ('reg, 'comm, 'rel, 'a) t = 'comm -> ('reg, 'rel) Argument.t array -> 'a
