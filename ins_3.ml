let rec add_to_each n lst =
  match lst with
  | [] -> []
  | h :: t -> (n :: h) :: add_to_each n t

let test1 = add_to_each 1 []

let test1 = add_to_each 1 [] = []

let test2 = add_to_each 1 [ [ 2 ] ]

let test2 = add_to_each 1 [ [ 2 ] ] = [ [ 1; 2 ] ]

let test3 = add_to_each 1 [ [ 2 ]; [ 2; 3 ] ]

let test3 = add_to_each 1 [ [ 2 ]; [ 2; 3 ] ] = [ [ 1; 2 ]; [ 1; 2; 3 ] ]

let test4 = add_to_each 1 [ [ 2 ]; [ 2; 3 ]; [ 2; 3; 4 ] ]

let test4 =
  add_to_each 1 [ [ 2 ]; [ 2; 3 ]; [ 2; 3; 4 ] ]
  = [ [ 1; 2 ]; [ 1; 2; 3 ]; [ 1; 2; 3; 4 ] ]

let rec prefix lst =
  match lst with
  | [] -> []
  | h :: t -> [h]::add_to_each h (prefix t)

let test5 = prefix [] = []

let test6 = prefix [ 1 ]
let test6 = prefix [ 1 ] = [ [ 1 ] ]

let test7 = prefix [ 1; 2 ]
let test7 = prefix [ 1; 2 ] = [ [ 1 ]; [ 1; 2 ] ]
