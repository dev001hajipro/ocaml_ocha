(* 問題 10.1 *)
let rec insert lst n =
  match lst with
  | [] -> [ n ]
  | h :: t -> if h < n then h :: insert t n else n :: lst

let test1 = insert [] 5 = [ 5 ]

let test1 = insert [ 3 ] 5 = [ 3; 5 ]

let test1 = insert [ 5 ] 3 = [ 3; 5 ]

let test1 = insert [ 1; 5 ] 3 = [ 1; 3; 5 ]

let test1 = insert [ 1; 2; 5 ] 3 = [ 1; 2; 3; 5 ]

let test1 = insert [ 1; 3; 4; 7; 8 ] 5 = [ 1; 3; 4; 5; 7; 8 ]

(* 問題　10.2 *)
let rec ins_sort = function
  | [] -> []
  | h :: t -> insert (ins_sort t) h

let test1 = ins_sort [ 5 ] = [ 5 ]

let test1 = ins_sort [ 5; 3 ] = [ 3; 5 ]

let test1 = ins_sort [ 1; 5; 3 ] = [ 1; 3; 5 ]

let test1 = ins_sort [ 5; 3; 8; 1; 7; 4 ] = [ 1; 3; 4; 5; 7; 8 ]
