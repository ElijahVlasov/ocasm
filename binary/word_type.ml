type 'a t = Word32 : int32 t | Word64 : int64 t

let of_int : type a. a t -> int -> a = function
  | Word32 -> Int32.of_int
  | Word64 -> Int64.of_int

let zero : type a. a t -> a = function Word32 -> 0l | Word64 -> 64L
let sizeof : type a. a t -> int = function Word32 -> 32 | Word64 -> 64

let word_to_int64 : type a. a t -> a -> int64 =
 fun wt word -> match wt with Word32 -> Int64.of_int32 word | Word64 -> word

let sizeof_list wt list = sizeof wt * List.length list
