type t_edge =
  { v1 : string
  ; v2 : string
  ; weight : float
  }

(* 辺 *)
let edges =
  [ { v1 = "A"; v2 = "B"; weight = 10. }
  ; { v1 = "A"; v2 = "D"; weight = 4. }
  ; { v1 = "B"; v2 = "C"; weight = 2. }
  ; { v1 = "B"; v2 = "E"; weight = 2. }
  ; { v1 = "C"; v2 = "E"; weight = 1. }
  ; { v1 = "D"; v2 = "E"; weight = 3. }
  ]

(* 頂点と辺の定義 *)
type t_vertex =
  { name : string
  ; distance : float
  ; route : string list
  }

let test1 =
  { name = "A"; distance = 1.; route = [ "A"; "B" ] }.route = [ "A"; "B" ]

let rec make_graph = function
  | [] -> []
  | h :: t -> { name = h; distance = infinity; route = [] } :: make_graph t

let test1 =
  make_graph [ "A"; "B" ]
  = [ { name = "A"; distance = infinity; route = [] }
    ; { name = "B"; distance = infinity; route = [] }
    ]

(** 始点の最短距離を0にする *)
let rec set_start name graph =
  match graph with
  | [] -> []
  | h :: t ->
    if name = h.name then { h with distance = 0. ;route=[name]} :: t
    else h :: set_start name t

let test1 = set_start "A" (make_graph [ "A"; "B"; "C" ])

let rec get_weight v1 v2 edges =
  match edges with
  | [] -> infinity
  | h :: t ->
    if (v1 = h.v1 && v2 = h.v2) || (v2 = h.v1 && v1 = h.v2) then h.weight
    else get_weight v1 v2 t

let get_distance v1 v2 edges = get_weight v1 v2 edges

let test1 = get_weight "A" "B" edges = 10.

let test1 = get_weight "D" "E" edges = 3.

let test1 = get_weight "E" "D" edges = 3.

let update_q p q edges =
  let p_q_dist = get_distance p.name q.name edges in
  if p_q_dist <> infinity then
    let q_via_p_dist = p.distance +. p_q_dist in
    (* 最短距離がp経由の方が小さい場合は更新 *)
    if q_via_p_dist < q.distance then
      { q with distance = q_via_p_dist; route = q.name :: p.route }
    else q
  else q

let test1 =
  let p = { name = "A"; distance = 0.; route = [] } in
  let q = { name = "B"; distance = 11.; route = [] } in
  update_q p q edges = { name = "B"; distance = 10.; route = [] }

let update p vs = List.map (fun q -> update_q p q edges) vs

let test1 =
  update
    { name = "A"; distance = 0.; route = [] }
    [ { name = "B"; distance = infinity; route = [] }
    ; { name = "D"; distance = infinity; route = [] }
    ]

let rec minimum lst =
  match lst with
  | [] -> max_int
  | h :: t ->
    let m = minimum t in
    if h < m then h else m

let test1 = minimum [ 1; 2; 3; 4; 5 ] = 1

let test1 = minimum [ 10; 8; 7; 3; 4; 5 ] = 3

(** 最短距離最小の要素を取得。*)
let rec shortest vs =
  match vs with
  | [] -> { name = ""; distance = infinity; route = [] }
  | h :: t ->
    let s = shortest t in
    if h.distance < s.distance then h else s

(** 最短距離最小の要素を取得。List.fold_rightで実装 *)
let rec shortest' vs =
  List.fold_right
    (fun h s -> if h.distance < s.distance then h else s)
    vs
    { name = ""; distance = infinity; route = [] }

let vs = make_graph [ "A"; "B"; "C" ]

let vs2 = set_start "A" vs

let s1 = shortest vs2

let vs3 = List.filter (fun x -> x <> s1) vs2

let s1 = shortest' vs2

let vs3 = List.filter (fun x -> x <> s1) vs2

let separate vs =
  let s = shortest vs in
  let vs_without_s = List.filter (fun x -> x <> s) vs in
  (s, vs_without_s)

let vs = make_graph [ "A"; "B"; "C" ]

let vs2 = set_start "A" vs

let tpl1 = separate vs2

let rec dijkstra_main vs =
  match vs with
  | [] -> []
  | _ ->
    let s, vs_without_s = separate vs in
    let vs' = update s vs_without_s in
    s :: dijkstra_main vs'

let vs = make_graph [ "A"; "B"; "C"; "D"; "E" ]

let vs2 = set_start "A" vs

let test1 = dijkstra_main vs2