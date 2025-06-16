type 'a t = Word32 : int32 t | Word64 : int64 t
(* A type of the machine word types *)

val sizeof : 'a t -> int
val sizeof_list : 'a t -> 'a list -> int
val word_to_int64 : 'a t -> 'a -> int64


