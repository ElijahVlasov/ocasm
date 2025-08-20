include module type of Command_intf

val instruction : 'instr -> ('instr, 'dir) t
val directive : 'dir -> ('instr, 'dir) t
val label : string -> ('instr, 'dir) t
