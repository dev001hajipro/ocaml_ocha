(* 整列されたリストに要素を追加する。 *)
(* val insert : int list -> int -> int list *)

let rec insert lst n =
  match lst with
  | [] -> [ n ]
  | h :: t -> if h < n then h :: insert t n else n :: h :: t

let test1 = insert [] 3 = [ 3 ]

let test2 = insert [ 5 ] 3

let test2 = insert [ 5 ] 3 = [ 3; 5 ]

let test3 = insert [ 3 ] 5

let test3 = insert [ 3 ] 5 = [ 3; 5 ]

let test4 = insert [ 1; 3; 4 ] 5

let test4 = insert [ 1; 3; 4 ] 5 = [ 1; 3; 4; 5 ]

(* 挿入ソート *)
let rec ins_sort lst =
  match lst with
  | [] -> []
  | h :: t -> insert (ins_sort t) h

let test1 = ins_sort [ 5; 3 ]

let test1 = ins_sort [ 5; 3 ] = [ 3; 5 ]

let test1 = ins_sort [ 5; 3; 8; 1; 7; 4 ] = [ 1; 3; 4; 5; 7; 8 ]
