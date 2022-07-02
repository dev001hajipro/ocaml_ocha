#use "./metro.ml"

(* 17 *)
type team_t =
  | Red
  | White

let term_string team =
  match team with
  | Red -> "紅組"
  | White -> "白組"

let test1 = term_string Red = "紅組"

(* 引数を持てる *)

(** 年号 *)
type nengou_t =
  | Meiji of int
  | Taisho of int
  | Showa of int
  | Heisei of int
  | Reiwa of int

let to_seireki nengou =
  match nengou with
  | Meiji n -> n + 1867
  | Taisho n -> n + 1911
  | Showa n -> n + 1925
  | Heisei n -> n + 1988
  | Reiwa n -> n + 2018

(* 事故参照していないケースが必ずあることを確認。*)
type t_tree =
  | Empty
  | Leaf of int
  | Node of t_tree * int * t_tree

let my_tree = Node (Node (Empty, 7, Leaf 3), 17, Leaf 24)

let tree1 = Empty

let tree2 = Leaf 3

let tree3 = Node (tree1, 4, tree2)

let tree4 = Node (tree2, 5, tree3)

let rec sum_tree tree =
  match tree with
  | Empty -> 0
  | Leaf n -> n
  | Node (t1, n, t2) -> sum_tree t1 + n + sum_tree t2

let test1 = sum_tree Empty = 0

let test2 = sum_tree (Leaf 5) = 5

let test3 = sum_tree (Node (Leaf 3, 3, Empty)) = 6

let test4 = sum_tree (Node (Leaf 3, 3, Leaf 3)) = 9

let test5 = sum_tree (Node (Leaf 3, 3, Node (Leaf 3, 3, Empty))) = 12

(* 17.5 *)
let rec tree_double = function
  | Empty -> Empty
  | Leaf n -> Leaf (n * 2)
  | Node (t1, n, t2) -> Node (tree_double t1, n * 2, tree_double t2)

let test1 = tree_double Empty = Empty

let test2 = tree_double (Leaf 5) = Leaf 10

let test3 = tree_double (Node (Leaf 3, 3, Empty)) = Node (Leaf 6, 6, Empty)

let test4 = tree_double (Node (Leaf 3, 3, Leaf 3)) = Node (Leaf 6, 6, Leaf 6)

let test5 =
  tree_double (Node (Leaf 3, 3, Node (Leaf 3, 3, Empty)))
  = Node (Leaf 6, 6, Node (Leaf 6, 6, Empty))

(* 17.6 *)
let rec tree_map f = function
  | Empty -> Empty
  | Leaf n -> Leaf (f n)
  | Node (t1, n, t2) -> Node (tree_map f t1, f n, tree_map f t2)

let test1 = tree_map (fun n -> n + 1) Empty = Empty

let test2 = tree_map (fun n -> n + 1) (Leaf 5) = Leaf 6

let test3 =
  tree_map (fun n -> n + 1) (Node (Leaf 3, 3, Empty)) = Node (Leaf 4, 4, Empty)

let test4 =
  tree_map (fun n -> n + 1) (Node (Leaf 3, 3, Leaf 3)) = Node (Leaf 4, 4, Leaf 4)

let test5 =
  tree_map (fun n -> n + 1) (Node (Leaf 3, 3, Node (Leaf 3, 3, Empty)))
  = Node (Leaf 4, 4, Node (Leaf 4, 4, Empty))

(* 17.7 *)
let rec tree_length = function
  | Empty -> 0
  | Leaf n -> 1
  | Node (t1, n, t2) -> tree_length t1 + 1 + tree_length t2

let test1 = tree_length Empty = 0

let test2 = tree_length (Leaf 5) = 1

let test3 = tree_length (Node (Leaf 3, 3, Empty)) = 2

let test4 = tree_length (Node (Leaf 3, 3, Leaf 3)) = 3

let test5 = tree_length (Node (Leaf 3, 3, Node (Leaf 3, 3, Empty))) = 4

(* 17.8 *)
let rec tree_depth = function
  | Empty -> 0
  | Leaf n -> 1
  | Node (t1, n, t2) ->
    let d1 = tree_depth t1 in
    let d2 = tree_depth t2 in
    if d1 < d2 then 1 + d2 else 1 + d1

let test1 = tree_depth Empty = 0

let test2 = tree_depth (Leaf 5) = 1

let test3 = tree_depth (Node (Leaf 3, 3, Empty)) = 2

let test4 = tree_depth (Node (Leaf 3, 3, Leaf 3)) = 2

let test5 = tree_depth (Node (Leaf 3, 3, Node (Leaf 3, 3, Empty))) = 3

(* 17.4 二分探索木 binary search tree *)
(* 節の左側のデータは、その節に格納されているデータより小さい。*)
(* 節の右側のデータは、その節に格納されているデータより大きい。*)

(** dataが二分探索木treeに含まれているか調べる。*)
let rec search tree data =
  match tree with
  | Empty -> false
  | Leaf n -> n = data
  | Node (t1, n, t2) ->
    if n = data then true
    else if data < n then search t1 data
    else search t2 data

(* 入力される二分探索木データ *)
let tree1 = Empty

let tree2 = Leaf 3

let tree3 = Node (Leaf 1, 2, Leaf 3)

let tree4 = Node (Empty, 7, Leaf 9)

let tree5 = Node (tree3, 6, tree4)

(* テスト *)
let test1 = search tree1 3 = false

let test2 = search tree2 3 = true

let test3 = search tree2 4 = false

let test4 = search tree5 6 = true

let test5 = search tree5 2 = true

let test6 = search tree5 1 = true

let test7 = search tree5 4 = false

let test8 = search tree5 7 = true

let test8 = search tree5 8 = false

let rec insert_tree tree data =
  match tree with
  | Empty -> Leaf data
  | Leaf n ->
    if data = n then Leaf n
    else if data < n then Node (Leaf data, n, Empty)
    else Node (Empty, n, Leaf data)
  | Node (t1, n, t2) ->
    if data = n then Node (t1, n, t2)
    else if data < n then Node (insert_tree t1 data, n, t2)
    else Node (t1, n, insert_tree t2 data)

let rec insert_tree' tree data =
  match tree with
  | Empty -> Leaf data
  | Leaf n when data < n -> Node (Leaf data, n, Empty)
  | Leaf n when data > n -> Node (Empty, n, Leaf data)
  | Leaf n -> Leaf n
  | Node (t1, n, t2) when data < n -> Node (insert_tree' t1 data, n, t2)
  | Node (t1, n, t2) when data > n -> Node (t1, n, insert_tree' t2 data)
  | Node (t1, n, t2) -> Node (t1, n, t2)

let tree4 = Node (Empty, 7, Leaf 9)

let test1 = insert_tree tree4 5

let tree5 = Node (Empty, 7, Leaf 9)

let test2 = insert_tree tree4 8

let tree5 = Node (Empty, 7, Leaf 9)

let test2 = insert_tree' tree4 10

(* 17.5 *)
type 'a tree_t =
  | Empty
  | Leaf of 'a
  | Node of 'a tree_t * 'a * 'a tree_t

(* type ('a, 'b) t_association_tree = | Empty | Leaf of 'a * 'b | Node of ('a,
   'b) t_association_tree * 'a * 'b * ('a, 'b) t_association_tree *)

(* 17.9 *)
let rec sum_tree' = function
  | Empty -> 0
  | Leaf n -> n
  | Node (t1, n, t2) -> sum_tree' t1 + n + sum_tree' t2

let test1 = sum_tree' (Node (Empty, 7, Leaf 9)) = 16

(* 停止で0を使い、加算が+を使っているので、多相型polymorphismにならない。 *)

(* 問題17.10 *)

type ekikan_tree_t =
  | Empty
  (* 漢字の駅名、(ここつながっている駅名, 距離) *)
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t

(* "茗荷谷" * [("新大塚",1.2);("後楽園",1.8)]*)

(* 問題17.11 *)
(* 連想リスト association list *)

(** 目的: 駅名と(駅名,距離)のリストを受け取ったら その距離を返す *)
let rec assoc name lst =
  match lst with
  | [] -> infinity
  | (n, kyori) :: t -> if name = n then kyori else assoc name t

let test1 = assoc "後楽園" [ ("新大塚", 1.2); ("後楽園", 1.8) ] = 1.8

let test2 = assoc "池袋" [ ("新大塚", 1.2); ("後楽園", 1.8) ] = infinity

(* 問題17.12 *)
(* metro.mlで定義 type ekikan_t = { kiten : string ; shuten : string ; keiyu :
   string ; kyori : float ; jikan : int } *)
(* 目的: ekikan_tree_t型の木とekikan_t型の駅間を受け取ったら その情報を挿入した木を返す。 *)
let rec insert_ekikan' ekikan_tree ekikan =
  match ekikan_tree with
  | Empty -> Node (Empty, ekikan.kiten, [ (ekikan.shuten, ekikan.kyori) ], Empty)
  | Node (left, ekimei, xs, right) ->
    if ekikan.kiten < ekimei then
      Node (insert_ekikan' left ekikan, ekimei, xs, right)
    else if ekikan.kiten > ekimei then
      Node (left, ekimei, xs, insert_ekikan' right ekikan)
    else Node (left, ekimei, (ekikan.shuten, ekikan.kyori) :: xs, right)

(* まずは挿入しないでtreeを返す。(再帰処理)*)

let ekikan1 =
  { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 }

let test1 = insert_ekikan' Empty ekikan1

let tree2 = Node (Empty, "秋葉原", [ ("御徒町", 1.0); ("上野", 3.0) ], Empty)

let test2 = insert_ekikan' tree2 ekikan1

(* 起点と終点を入れ替える。*)
let swap_first_last ekikan =
  { ekikan with kiten = ekikan.shuten; shuten = ekikan.kiten }

let rec insert_ekikan ekikan_tree ekikan =
  let swaped_ekikan = swap_first_last ekikan in
  insert_ekikan' (insert_ekikan' ekikan_tree swaped_ekikan) ekikan

let test1 = insert_ekikan tree2 ekikan1

(* 問題17.13 *)
let rec inserts_ekikan ekikan_tree ekikan_list =
  match ekikan_list with
  | [] -> Empty
  | h :: t -> insert_ekikan (inserts_ekikan ekikan_tree t) h

let rec inserts_ekikan2 ekikan_tree ekikan_list =
  List.fold_right (fun a tree -> insert_ekikan tree a) ekikan_list ekikan_tree

let test1 = inserts_ekikan2 Empty global_ekikan_list

let rec insserts_ekikan ekikan_tree ekikan_list =
  List.fold_left
    (fun tree ekikan -> insert_ekikan tree ekikan)
    ekikan_tree ekikan_list

(* todo: テスト追加 *)
(* 問題17.14 *)
(* 木を使ったget_ekikan_kyori *)
(* 目的: 漢字の駅名2つとekikan_tree_t型の木を受け取ったら、距離を返す。*)
let rec get_ekikan_kyori eki1_name eki2_name ekikan_tree =
  match ekikan_tree with
  | Empty -> infinity
  | Node (left, name, connected_list, right) ->
    if eki1_name < name then get_ekikan_kyori eki1_name eki2_name left
    else if eki1_name > name then get_ekikan_kyori eki1_name eki2_name right
    else assoc eki2_name connected_list

let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list

let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_tree = 1.2

let test2 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_tree = infinity

let test3 = get_ekikan_kyori "東京" "大手町" global_ekikan_tree = 0.6
