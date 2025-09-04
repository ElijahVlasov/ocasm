open! Import

module Diagnostics_type = struct
  type t = Warning | Error [@@deriving eq, show { with_path = false }]
end

type t = {
  id : string;
  ty : Diagnostics_type.t;
  msg : string;
  starts : Location.t;
  ends : Location.t;
  file : Path.t;
  ctx : string;
}
