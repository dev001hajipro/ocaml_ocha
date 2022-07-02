let rec sum lst = match lst with
[] -> 0
|h::t -> h + sum t

let test1 = sum [1;2;3;4;5]
(* fold_right is "folding in" elements of the list from the right to the left.
1 + (2 + (3 + (4 + (5 + 0))))
*)

let rec concat lst = match lst with
[] -> ""
|h::t -> h ^ concat t
let test2 = concat ["a";"b";"c";"d";"e"]

let rec combine f init lst = match lst with
[] -> init
|h::t -> f h (combine f init t)

let sum' = combine (+) 0
let test3 = sum' [1;2;3;4;5]

let concat' = combine (^) ""
let test4 = concat' ["a";"b";"c";"d";"e"]

let rec fold_right f init lst = match lst with
[] -> init
|h::t -> f h (fold_right f init t)

let sum'' = fold_right (+) 0

let test5 = sum'' [1;2;3;4;5]

(* fold_left is "folding in" elements of the list from the left to right, 
combining each new element using the operator. 
*)
let rec fold_left f acc lst = match lst with
[] -> acc
|h::t -> fold_left f (f h acc) t

let sum''' = fold_left (+) 0
let test6 = sum''' [1;2;3;4;5]

(* fold in left to right. 
 (+ 5(+ 4(+ 3(+ 2 (+ 1 0))))) 0)
*)

