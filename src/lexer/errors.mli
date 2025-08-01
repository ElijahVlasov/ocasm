include module type of Errors_intf

module Recovery : sig
  type t = recovery
end

module Error : sig
  type 'a t = Error : error -> 'a t

  include To_diagnostic_message.S with type 'a t := 'a t
end

module Warning : sig
  type 'a t = Warning : warning -> unit t

  include To_diagnostic_message.S with type 'a t := 'a t
end
