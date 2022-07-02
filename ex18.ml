(* ************************************************************************** 
 * 第18章 例外と例外処理 
 * ************************************************************************** *)
let yaoya_list = [ ("トマト", 300); ("玉ねぎ", 200); ("人参", 150); ("ほうれん草", 200) ]

(* option型を使う。 *)
let rec price item yaoya_list =
  match yaoya_list with
  | [] -> None
  | (yasai, nedan) :: t -> if item = yasai then Some nedan else price item t

let test1 = price "トマト" yaoya_list = Some 300

let test2 = price "玉ねぎ" yaoya_list = Some 200

let test3 = price "トマト" [] = None

(* ************************************************************************** *)
(* 18.1 オプション型 *)
let yaoya_list = [ ("トマト", 300); ("玉ねぎ", 200); ("人参", 150); ("ほうれん草", 200) ]

let rec price item = function
  | [] -> None
  | (yasai, nedan) :: t -> if item = yasai then Some nedan else price item t

let test1 = price "トマト" yaoya_list = Some 300

let test1 = price "バナナ" yaoya_list = None

(* 問題18.1 *)
type person_t =
  { name : string
  ; height : float
  ; weight : float
  ; birthday : string
  ; blood : string
  }

let persons =
  [ { name = "matsui"
    ; height = 173.
    ; weight = 60.
    ; birthday = "20101010"
    ; blood = "B"
    }
  ; { name = "ozaki"
    ; height = 173.
    ; weight = 60.
    ; birthday = "20101010"
    ; blood = "O"
    }
  ; { name = "tanaka"
    ; height = 173.
    ; weight = 60.
    ; birthday = "20101010"
    ; blood = "A"
    }
  ; { name = "yamada"
    ; height = 173.
    ; weight = 60.
    ; birthday = "20101010"
    ; blood = "AB"
    }
  ; { name = "sato"
    ; height = 173.
    ; weight = 60.
    ; birthday = "20101010"
    ; blood = "A"
    }
  ]

let rec first_A = function
  | [] -> None
  | h :: t -> if h.blood = "A" then Some h else first_A t

let test1 =
  first_A persons
  = Some
      { name = "tanaka"
      ; height = 173.
      ; weight = 60.
      ; birthday = "20101010"
      ; blood = "A"
      }

(* 18.2 オプション型を使った例外処理 *)
let yaoya_list = [ ("トマト", 300); ("玉ねぎ", 200); ("人参", 150); ("ほうれん草", 200) ]

let rec total_price yasai_list yaoya_list =
  match yasai_list with
  | [] -> 0
  | yasai :: t -> (
    match price yasai yaoya_list with
    | None -> (* 八百屋に野菜がなかったとき *) total_price t yaoya_list
    | Some p -> p + total_price t yaoya_list)

let test1 = total_price [] yaoya_list = 0

let test2 = total_price [ "玉ねぎ" ] yaoya_list = 200

let test3 = total_price [ "玉ねぎ"; "トマト" ] yaoya_list = 500

let test4 = total_price [ "じゃがいも" ] yaoya_list = 0

let test5 = total_price [ "じゃがいも"; "トマト" ] yaoya_list = 300

(* 問題18.2 *)
let rec count_urikire_yasai yasai_list yaoya_list =
  match yasai_list with
  | [] -> 0
  | namae :: t -> (
    match price namae yaoya_list with
    | None -> 1 + count_urikire_yasai t yaoya_list
    | Some _ -> count_urikire_yasai t yaoya_list)

let test1 = count_urikire_yasai [] yaoya_list = 0

let test2 = count_urikire_yasai [ "玉ねぎ" ] yaoya_list = 0

let test3 = count_urikire_yasai [ "玉ねぎ"; "トマト" ] yaoya_list = 0

let test4 = count_urikire_yasai [ "じゃがいも" ] yaoya_list = 1

let test5 = count_urikire_yasai [ "じゃがいも"; "トマト" ] yaoya_list = 1

(* 18.3 *)

(* 元々のtotal_priceは、戻り値がint 内部で使っているprice関数はint option型を返す。 *)
let rec total_price yasai_list yaoya_list =
  match yasai_list with
  | [] -> 0
  | namae :: t -> (
    match price namae yaoya_list with
    | None -> 0 + total_price t yaoya_list
    | Some p -> p + total_price t yaoya_list)
(* ここがp + 関数のようにint型の処理 *)

(* ひとつでも買えない野菜があったら買わない。 total_priceがint option型を返す実装にする。 *)
let rec total_price' yasai_list yaoya_list =
  match yasai_list with
  | [] -> Some 0
  | namae :: t -> (
    match price namae yaoya_list with
    | None -> None (* 野菜がない場合は買わない。 *)
    | Some p -> (
      match total_price' t yaoya_list with
      | None -> None (* 上記の呼び出し時の内部price関数でNoneが返ってくる場合。*)
      | Some total -> Some (p + total)))

(*
 * 上記でint option型を返すtotal_price'ができたので、これを補助関数にしてint型を返す。
 *)
let rec total_price'' yasai_list yaoya_list =
  let rec aux yasai_list yaoya_list =
    match yasai_list with
    | [] -> Some 0
    | namae :: t -> (
      match price namae yaoya_list with
      | None -> None (* 野菜がない場合は買わない。 *)
      | Some p -> (
        match aux t yaoya_list with
        | None -> None (* 上記の呼び出し時の内部price関数でNoneが返ってくる場合。*)
        | Some total -> Some (p + total)))
  in
  match aux yasai_list yaoya_list with
  | None -> 0
  | Some p -> p

let test1 = total_price'' [ "玉ねぎ"; "トマト" ] yaoya_list = 500

let test2 = total_price'' [ "じゃがいも" ] yaoya_list = 0

let test3 = total_price'' [ "じゃがいも"; "トマト" ] yaoya_list = 0

(* 18.4 例外処理専用の構文 *)

exception Soldout of string * int

exception Urikire

(* 18.5 例外処理の実際 *)
let rec price item yaoya_list =
  match yaoya_list with
  | [] -> raise Urikire
  | (namae, nedan) :: t -> if item = namae then nedan else price item t

let yaoya_list = [ ("トマト", 300); ("玉ねぎ", 200); ("人参", 150); ("ほうれん草", 200) ]

let test1 = price "トマト" yaoya_list = 300

(* let test2 = price "じゃがいも" yaoya_list = Urikire *)

let rec total_price yasai_list yaoya_list =
  let rec aux = function
    | [] -> 0
    | h :: t -> price h yaoya_list + aux t
  in
  try aux yasai_list with
  | Urikire -> 0

let test1 = total_price [ "じゃがいも" ] yaoya_list = 0

let test2 = total_price [ "人参"; "玉ねぎ" ] yaoya_list = 350

(* 問題18.3 *)
(* 問題17.11で作成したassocで、キーが見つからないときはNot_found例外を返す。*)
let rec assoc name lst =
  match lst with
  | [] -> raise Not_found
  | (n, kyori) :: t -> if n = name then kyori else assoc name t

let test1 = assoc "ueno" [ ("okachimachi", 1.3); ("ueno", 0.2) ] = 0.2

let test2 =
  (try assoc "akihabara" [ ("okachimachi", 1.3); ("ueno", 0.2) ] with
  | Not_found -> infinity)
  = infinity

(* 18.6 例外処理を使ったプログラミング *)
let rec times = function
  | [] -> 1
  | h :: t -> h * times t

(* 引数のリスト内に0がある場合に対応する。*)
exception Zero

let rec times lst =
  let aux = function
    | [] -> 1
    | h :: t -> if h = 0 then raise Zero else h * times t
  in
  try aux lst with
  | Zero -> 0

let test1 = times [ 0; 1; 2; 3; 4; 5 ] = 0

let test2 = times [ 1; 2; 3; 4; 5 ] = 120;;

#mod_use "./tree/tree.ml"

let a = Tree.empty
