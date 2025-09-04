open! Import
include module type of Diagnostics_message_intf
include Pretty_printer.S with type t := t
