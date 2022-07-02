let rec append xs ys =
  match xs with
  | [] -> ys
  | h :: t -> h :: append t ys

let test1 = append [] [] = []

let test2 = append [] [ 1; 2 ] = [ 1; 2 ]

let test3 = append [ 1; 2 ] [] = [ 1; 2 ]

let test4 = append [ 1; 2 ] [ 3; 4 ] = [ 1; 2; 3; 4 ]

let test5 = append [ "a"; "b" ] [ "f"; "g" ] = [ "a"; "b"; "f"; "g" ]

let rec append' xs ys = List.fold_right (fun a acc -> a :: acc) xs ys

let test1 = append' [] [] = []

let test2 = append' [] [ 1; 2 ] = [ 1; 2 ]

let test3 = append' [ 1; 2 ] [] = [ 1; 2 ]

let test4 = append' [ 1; 2 ] [ 3; 4 ] = [ 1; 2; 3; 4 ]

let test5 = append' [ "a"; "b" ] [ "f"; "g" ] = [ "a"; "b"; "f"; "g" ]
