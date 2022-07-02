let rec map f lst =
  match lst with
  | [] -> []
  | h :: t -> f h :: map f t

let test1 = map (fun a -> a + 1) [ 1; 2; 3; 4; 5 ] = [ 2; 3; 4; 5; 6 ]

let rec filter f = function
  | [] -> []
  | h :: t -> if f h = false then filter f t else h :: filter f t

let test1 = filter (fun a -> a mod 2 = 0) [ 1; 2; 3; 4; 5 ] = [ 2; 4 ]

let test2 = filter (fun a -> a mod 2 = 0) [ 1; 3; 5 ] = []

let rec ins n lst =
  match lst with
  | [] -> [ n ]
  | h :: t -> if n < h then n :: lst else (* h < n *) h :: ins n t

let rec sort lst =
  match lst with
  | [] -> []
  | h :: t -> ins h (sort t)

let test1 = sort [ 3 ] = [ 1; 3 ]

let test1 = sort [ 3 ] = [ 3 ]

let test2 = sort [ 5; 3 ] = [ 3; 5 ]

let test2 = sort [ 2; 5; 3 ] = [ 2; 3; 5 ]

let test2 = sort [ 4; 2; 5; 3 ] = [ 2; 3; 4; 5 ]

(* f1 f2 f3 f4 f5 0 *)
let rec fold_left f init lst =
  match lst with
  | [] -> init
  | h :: t -> f h (fold_left f init t)

let test1 = fold_left (fun a acc -> a + acc) 0 [ 1; 2; 3; 4; 5 ] = 15

let test1 = fold_left (fun a acc -> a + acc) 3 [] = 3

let rec fold_right f acc = function
  | [] -> acc
  | h :: t -> fold_right f (f h acc) t

let test1 = fold_right (fun a acc -> a::acc) [] [1;2;3] = [3;2;1]
let test2 = fold_right (fun a acc -> a +. acc) 10. [1.;2.;3.] = 16.