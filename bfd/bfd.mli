open Base

type bfd
type asection
type asymbol

module Error : sig
  type t =
    | NoError
    | SystemCall
    | InvalidTarget
    | WrongFormat
    | WrongObjectFormat
    | InvalidOperation
    | NoMemory
    | NoSymbols
    | NoArmap
    | NoMoreArchivedFiles
    | MalformedArchive
    | MissingDso
    | FileNotRecognized
    | FileAmbiguouslyRecognized
    | NoContents
    | NonrepresentableSection
    | NoDebugSection
    | BadValue
    | FileTruncated
    | FileTooBig
    | Sorry
    | OnInput
    | InvalidErrorCode

  include Equal.S with type t := t

  val to_string : t -> string
end

exception BfdException of Error.t

module Section_flags = Section_flags
module Symbol_flags = Symbol_flags

module BfdMonad : sig
  type 'a t

  include Base.Monad.S with type 'a t := 'a t

  val run : 'a t -> bfd -> 'a
  val ask : bfd t
end

val with_bfd : string -> string -> 'a BfdMonad.t -> 'a
val set_object_format : unit BfdMonad.t
val make_section : string -> asection BfdMonad.t
val set_section_flags : asection -> Section_flags.t -> unit
val set_section_size : asection -> int64 -> unit

type 'a word_type = Word32 : int32 word_type | Word64 : int64 word_type

val set_section_contents :
  'a word_type -> asection -> 'a list -> int64 -> unit BfdMonad.t

val make_symbol :
  string -> asection -> Symbol_flags.t -> int64 -> asymbol BfdMonad.t

val set_symtab : asymbol list -> unit BfdMonad.t
