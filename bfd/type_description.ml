open Ctypes

module Types (F : TYPE) = struct
  open F

  let bfd_arch_size = constant "BFD_ARCH_SIZE" size_t
  let bfd_init_magic = constant "BFD_INIT_MAGIC" size_t

  type bfdm
  type bfd = bfdm Ctypes.structure

  let bfd : bfd structure typ = structure "bfd"
  let bfd_filename = field bfd "filename" string
  let bfd_section_count = field bfd "section_count" int
  let bfd_error_no_error = constant "bfd_error_no_error" int64_t
  let bfd_error_system_call = constant "bfd_error_system_call" int64_t
  let bfd_error_invalid_target = constant "bfd_error_invalid_target" int64_t
  let bfd_error_wrong_format = constant "bfd_error_wrong_format" int64_t

  let bfd_error_wrong_object_format =
    constant "bfd_error_wrong_object_format" int64_t

  let bfd_error_invalid_operation =
    constant "bfd_error_invalid_operation" int64_t

  let bfd_error_no_memory = constant "bfd_error_no_memory" int64_t
  let bfd_error_no_symbols = constant "bfd_error_no_symbols" int64_t
  let bfd_error_no_armap = constant "bfd_error_no_armap" int64_t

  let bfd_error_no_more_archived_files =
    constant "bfd_error_no_more_archived_files" int64_t

  let bfd_error_malformed_archive =
    constant "bfd_error_malformed_archive" int64_t

  let bfd_error_missing_dso = constant "bfd_error_missing_dso" int64_t

  let bfd_error_file_not_recognized =
    constant "bfd_error_file_not_recognized" int64_t

  let bfd_error_file_ambiguously_recognized =
    constant "bfd_error_file_ambiguously_recognized" int64_t

  let bfd_error_no_contents = constant "bfd_error_no_contents" int64_t

  let bfd_error_nonrepresentable_section =
    constant "bfd_error_nonrepresentable_section" int64_t

  let bfd_error_no_debug_section = constant "bfd_error_no_debug_section" int64_t
  let bfd_error_bad_value = constant "bfd_error_bad_value" int64_t
  let bfd_error_file_truncated = constant "bfd_error_file_truncated" int64_t
  let bfd_error_file_too_big = constant "bfd_error_file_too_big" int64_t
  let bfd_error_sorry = constant "bfd_error_sorry" int64_t
  let bfd_error_on_input = constant "bfd_error_on_input" int64_t

  let bfd_error_invalid_error_code =
    constant "bfd_error_invalid_error_code" int64_t

  type bfd_error_type =
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
  [@@deriving eq, show]

  let bfd_error_type =
    enum "bfd_error"
      [
        (NoError, bfd_error_no_error);
        (SystemCall, bfd_error_system_call);
        (InvalidTarget, bfd_error_invalid_target);
        (WrongFormat, bfd_error_wrong_format);
        (WrongObjectFormat, bfd_error_wrong_object_format);
        (InvalidOperation, bfd_error_invalid_operation);
        (NoMemory, bfd_error_no_memory);
        (NoSymbols, bfd_error_no_symbols);
        (NoArmap, bfd_error_no_armap);
        (NoMoreArchivedFiles, bfd_error_no_more_archived_files);
        (MalformedArchive, bfd_error_malformed_archive);
        (MissingDso, bfd_error_missing_dso);
        (FileNotRecognized, bfd_error_file_not_recognized);
        (FileAmbiguouslyRecognized, bfd_error_file_ambiguously_recognized);
        (NoContents, bfd_error_no_contents);
        (NonrepresentableSection, bfd_error_nonrepresentable_section);
        (NoDebugSection, bfd_error_no_debug_section);
        (BadValue, bfd_error_bad_value);
        (FileTruncated, bfd_error_file_truncated);
        (FileTooBig, bfd_error_file_too_big);
        (Sorry, bfd_error_sorry);
        (OnInput, bfd_error_on_input);
        (InvalidErrorCode, bfd_error_invalid_error_code);
      ]

  let bfd_object = constant "bfd_object" int64_t

  type asectionm
  type asection = asectionm Ctypes.structure

  let asection : asection structure typ = structure "asection"
end
