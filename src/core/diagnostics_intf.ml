open! Import

module Warning = struct
  type t = ..
end

module Error = struct
  type t = ..
end

module Any = struct
  type t = Warning of Warning.t | Error of Error.t
end
