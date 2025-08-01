module type Immediate = sig
  type t

  val to_int32 : t -> int32
  val of_int32 : int32 -> t option
  val of_int32_unchecked : int32 -> t
  val to_int32_unchecked : t -> int32
  val eq_t : t -> t -> bool
  val to_string : t -> string
  val pp_t : Format.formatter -> t -> unit
end

module MkImmediate (M : sig
  val mask : int32
end) : Immediate = struct
  type t = int32

  let msb_mask = Int32.logxor M.mask (Int32.shift_right_logical M.mask 1)
  let mask_inv = Int32.lognot M.mask
  let is_negative imm = Int32.logand imm msb_mask <> 0l
  let to_int32 imm = if is_negative imm then Int32.logor imm mask_inv else imm
  let of_int32_unchecked x = x
  let to_int32_unchecked x = x
  let eq_t = Int32.equal
  let to_string = Int32.to_string
  let pp_t fmt imm = Format.fprintf fmt "%s" (to_string imm)

  let of_int32 x =
    if x < 0l then
      if Int32.abs x > Int32.shift_right_logical M.mask 1 then None
      else Some (Int32.logxor x mask_inv)
    else if x > Int32.shift_right_logical M.mask 1 then None
    else Some x
end

module Immediate12 : Immediate = MkImmediate (struct
  let mask = 0xfffl
end)

type imm12 = Immediate12.t

let equal_imm12 = Immediate12.eq_t
let pp_imm12 = Immediate12.pp_t
let zero_imm12 = Option.get (Immediate12.of_int32 0l)
let one_imm12 = Option.get (Immediate12.of_int32 1l)

module Immediate20 : Immediate = MkImmediate (struct
  let mask = 0x1fffffl
end)

type imm20 = Immediate20.t

let equal_imm20 = Immediate20.eq_t
let pp_imm20 = Immediate20.pp_t
