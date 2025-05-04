type bfd
type asection

exception BfdException of C.Types.bfd_error_type

module BfdMonad : sig
  type 'a t

  include Base.Monad.S with type 'a t := 'a t

  val run : 'a t -> bfd -> 'a
  val ask : bfd t
end

val with_bfd : string -> string -> 'a BfdMonad.t -> 'a
val set_object_format : bool BfdMonad.t
val make_section : string -> asection BfdMonad.t
val set_section_flags : asection -> Section_flags.t -> bool
val set_section_size : asection -> int64 -> bool

val set_section_contents :
  asection -> 'a list -> int64 -> 'a Ctypes.typ -> bool BfdMonad.t
