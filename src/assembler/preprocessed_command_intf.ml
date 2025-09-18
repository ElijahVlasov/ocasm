open! Import

type ('instr, 'dir) t = Instruction of 'instr | Directive of 'dir
