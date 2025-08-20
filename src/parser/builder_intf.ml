open! Import

module Builder_fn = struct
  type ('reg, 'comm, 'rel, 'a) t = 'comm -> ('reg, 'rel) Argument.t array -> 'a
end
