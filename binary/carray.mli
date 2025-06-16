open Ctypes

type 'a carray
type 'a word_type = Word32 : int32 word_type | Word64 : int64 word_type

module WordType : sig
  type 'a t = 'a word_type

  val sizeof : 'a t -> int
  val sizeof_list : 'a word_type -> 'a list -> int
  val to_typ : 'a t -> 'a typ
  val word_to_int64 : 'a t -> 'a -> int64
end

val of_list : 'a word_type -> 'a list -> 'a carray
val start : 'a carray -> 'a ptr
