(* 整列したリストに要素を追加する。*)
let rec insert lst n =
  match lst with
  | [] -> [ n ]
  | h :: t -> if h < n then h :: insert t n else n :: h :: t

let test1 = insert [] 5

let test2 = insert [ 1 ] 5

let test3 = insert [ 6 ] 5

let test4 = insert [ 1; 3; 8; 14; 21 ] 7

(* 挿入ソート *)
let rec ins_sort lst =
  match lst with
  | [] -> []
  | h :: t -> insert (ins_sort t) h

  let test1 = ins_sort [4]
  let test2 = ins_sort [4;3]
  (*
ins_sort [4;3]
4::[3] -> insert (ins_sort [3]) 4
4::[3] -> insert (3::[]->insert (ins_sort []) 3) 4
4::[3] -> insert (3::[]->insert [] 3) 4
4::[3] -> insert (3::[]->[3]) 4
4::[3] -> insert [3] 4
4::[3] -> then 3 :: insert [] 4
4::[3] -> 3 :: [4]
[3;4]
  *)
  let test3 = ins_sort [10;4;7;3]

  let test3 = ins_sort [30;48;2;10;4;77;65]
