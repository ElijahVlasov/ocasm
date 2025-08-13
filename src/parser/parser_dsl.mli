open! Import
include module type of Parser_dsl_intf

type ('reg, 'dir, 'opcode, 'res) t

module Selector : sig
  type ('reg, 'dir, 'opcode, 'res, 'a) t

  val res : ('reg, 'dir, 'opcode, 'res, 'res) t
  val dir : ('reg, 'dir, 'opcode, 'res, 'dir) t
  val opcode : ('reg, 'dir, 'opcode, 'res, 'opcode) t
end

val create :
  ?path:Path.t ->
  'reg Isa.Register.t ->
  'dir Isa.Expr.t ->
  'opcode Isa.Expr.t ->
  'res Isa.Expr.t ->
  ('reg, 'dir, 'opcode, 'res) t

val arg_type :
  ('reg, 'dir, 'opcode, 'res) t ->
  ('reg, 'dir, 'opcode, 'res, 'a) Selector.t ->
  'a ->
  Isa.Type.t list

val path : ('reg, 'dir, 'opcode, 'res) t -> Path.t
