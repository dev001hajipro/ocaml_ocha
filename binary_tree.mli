type 'a t =
  | Empty
  | Leaf of 'a
  | Node of 'a t * 'a * 'a t

val search : 'a t -> 'a -> 'a

val insert : 'a t -> 'a -> 'a t
