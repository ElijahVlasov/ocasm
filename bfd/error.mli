open Base

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
val get_error : unit -> t
