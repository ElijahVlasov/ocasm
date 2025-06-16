type 'a t = Word32 : int32 t | Word64 : int64 t

let sizeof : type a. a t -> int = function Word32 -> 32 | Word64 -> 64

let word_to_int64 : type a. a t -> a -> int64 =
 fun word_type word ->
  match word_type with Word32 -> Int64.of_int32 word | Word64 -> word

let sizeof_list word_type list = sizeof word_type * List.length list
