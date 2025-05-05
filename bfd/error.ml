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
[@@deriving eq]

let equal = equal
let to_string = C.Functions.bfd_errmsg
let get_error = C.Functions.bfd_get_error
let no_error = C.Types.equal_bfd_error_type C.Types.NoError
