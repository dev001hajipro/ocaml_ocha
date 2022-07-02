(* https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/hop/summary.html *)

let rec sum lst = match lst with [] -> 0 | h :: t -> h + sum t

let rec concat lst = match lst with [] -> "" | h :: t -> h ^ concat t

let test1 = sum [ 1; 2; 3; 4; 5 ]

let test2 = concat [ "hello"; "world" ]

let rec sum' init lst = match lst with [] -> init | h :: t -> h + sum' init t

let test1 = sum' 5 [ 1; 2; 3; 4; 5 ]

let rec concat' init = function [] -> init | h :: t -> h ^ concat' init t

let test1 = concat' "x" [ "hello"; "world" ]

let rec combine op init lst = match lst with
[] -> init
| h::t -> op h (combine op init t)

let sum'' = combine (+) 0
let concat'' = combine (^) ""



let rec fold_right op lst init = match lst with
| [] -> init
| h::t -> op h (fold_right op t init)

let sum''' lst = fold_right (+) lst 0
let comcat''' lst = fold_right (^) lst ""

let rec fold_left op acc lst = match lst with
[] -> acc
| h::t -> fold_left op (op acc h) t