let rec minimum lst =
  match lst with
  | [] -> max_int
  | h :: t -> if h < minimum t then h else minimum t

let test1 = minimum [ 1 ] = 1

let test2 = minimum [ 2; 1 ] = 1

let test3 = minimum [ 2; 3 ] = 2

let test4 = minimum [ 3; 2; 1 ] = 1

let rec minimum' xs =
  List.fold_right (fun a init -> if a < init then a else init) xs max_int

let test1 = minimum' [ 1 ] = 1

let test2 = minimum' [ 2; 1 ] = 1

let test3 = minimum' [ 2; 3 ] = 2

let test4 = minimum' [ 3; 2; 1 ] = 1