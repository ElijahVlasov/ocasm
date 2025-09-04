open Import

module Error = struct
  type t =
    | Incorrect_escape_sequence of char
    | Incorrect_hex_escape_sequence of char * char
    | Unfinished_comment
    | Unfinished_string_literal
    | Expected_vs_got of char * char
    | Wrong_char_in_number_literal of char
    | Junk_symbol of char
end

module Warning = struct
  type t = Newline_in_string_literal
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
