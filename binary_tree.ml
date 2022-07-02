type 'a t =
  | Empty
  | Leaf of 'a
  | Node of 'a t * 'a * 'a t

let rec search tree x =
  match tree with
  | Empty -> x
  | Leaf y -> y
  | Node (left, n, right) ->
    if n = x then n else if x < n then search left x else search right x

let rec insert tree data =
  match tree with
  | Empty -> Leaf data
  | Leaf n ->
    if data = n then Leaf n
    else if data < n then Node (Leaf data, n, Empty)
    else Node (Empty, n, Leaf data)
  | Node (left, n, right) ->
    if n = data then Node (left, n, right)
    else if data < n then Node (insert left data, n, right)
    else Node (left, data, insert right data)
