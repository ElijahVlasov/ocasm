open Ctypes

type 'a carray = 'a Ctypes.carray
type 'a word_type = Word32 : int32 word_type | Word64 : int64 word_type

module WordType : sig
  type 'a t = 'a word_type

  val to_typ : 'a t -> 'a typ
  val sizeof : 'a t -> int
  val word_to_int64 : 'a t -> 'a -> int64
  val sizeof_list : 'a word_type -> 'a list -> int
end = struct
  type 'a t = 'a word_type

  let to_typ : type a. a t -> a typ = function
    | Word32 -> int32_t
    | Word64 -> int64_t

  let sizeof : type a. a t -> int =
   fun word_type -> Ctypes.sizeof (to_typ word_type)

  let word_to_int64 : type a. a t -> a -> int64 =
   fun word_type word ->
    match word_type with Word32 -> Int64.of_int32 word | Word64 -> word

  let sizeof_list word_type list = sizeof word_type * List.length list
end

let of_list : type a. a word_type -> a list -> a carray =
 fun word_type list -> CArray.of_list (WordType.to_typ word_type) list

let start = CArray.start
