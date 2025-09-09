open Import

module Error = struct
  type t = Not_a_command of string
end

module Warning = struct
  type t = Test
end

module With_id = struct
  module type S = sig
    type t

    val id : t -> string
  end
end

module type Intf = sig
  module With_id = With_id

  module Error : sig
    include module type of Error

    val show : t -> string

    include With_id.S with type t := t
  end

  module Warning : sig
    include module type of Warning

    val show : t -> string

    include With_id.S with type t := t
  end
end
