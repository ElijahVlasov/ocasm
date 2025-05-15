open Ctypes

type 'a carray
type 'a word_type = Word32 : int32 word_type | Word64 : int64 word_type

module WordType : sig
  type 'a t = 'a word_type

  val to_typ : 'a t -> 'a typ
end

val of_list : 'a word_type -> 'a list -> 'a carray
val start : 'a carray -> 'a ptr
