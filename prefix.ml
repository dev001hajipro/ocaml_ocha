(* 接頭語のリストを受け取ったら、先頭にもうひとつ要素を付け加える。 *)
let rec add_to_each n lst =
  match lst with
  | [] -> []
  | h :: t -> (n :: h) :: add_to_each n t

let test4 = add_to_each 1 [ [ 2 ]; [ 2; 3 ]; [ 2; 3; 4 ] ]

(* val prefix : 'a list -> 'alist list *)
(* 解き方のポイントとしては、テストケースを書いて、上から消化していくと
 * [x] :: add_to_each x (prefix x)のかたちが導き出せる。
*)
let rec prefix lst =
  match lst with
  | [] -> []
  | h :: t -> [ h ] :: add_to_each h (prefix t)

let test5 = prefix []

let test5 = prefix [] = []

let test6 = prefix [ 1 ]

let test6 = prefix [ 1 ] = [ [ 1 ] ]

let test7 = prefix [ 1; 2 ]

let test7 = prefix [ 1; 2 ] = [ [ 1 ]; [ 1; 2 ] ]

let test8 = prefix [ 1; 2; 3; 4 ]

let test8 =
  prefix [ 1; 2; 3; 4 ] = [ [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ]; [ 1; 2; 3; 4 ] ]
