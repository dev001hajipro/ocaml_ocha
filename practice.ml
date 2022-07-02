#use "./metro.ml"

(* *********************************** *)
(*4.1 *)
(* 年とその月の働いた時間が与えられたら
 * その月の給与を返す。
 *)
(* int -> int -> int *)
let baito_kyuyo y t =
  let jikyu = (y * 100) + 850 in
  jikyu * t

let baito_kyuyo y t = ((y * 100) + 850) * t

let test1 = baito_kyuyo 0 2 = 1700

let test1 = baito_kyuyo 1 2 = 1900

let test2 = baito_kyuyo 2 2 = 2100

let test3 = baito_kyuyo 1 30 = 950 * 30

(* *********************************** *)
(*4.2 *)
let jikosyoukai name = "こんにちは " ^ name ^ "です。" ^ "Ocamlでいろんなプログラミングをしていきたいです。"

(* *********************************** *)
(*4.3 *)
let hyojun_taiju m = m * m * 22

(* *********************************** *)
(*4.4 *)
let bmi m kg = kg /. (m *. m)

(* 関数型の右結合 *)
let g x y = (x * x) + (y * y) - 4

(* val g : int -> int -> int = <fun> *)
(* int -> (int -> int)　の意味。 A -> (B -> C ) なら、Aという引数１つの関数で、(B-C)関数を返す。という意味。 *)

(*4.5 いままで作った関数の方は? *)
(* A. REPLで確認できる。 *)

(* *********************************** *)
(* 4.6 *)
(* 鶴の数を与えられたら、足の本数を返す。*)
(* val tsuru_no_ashi : int -> int = <fun> *)
let tsuru_no_ashi n = n * 2

let test1 = tsuru_no_ashi 0 = 0

let test2 = tsuru_no_ashi 1 = 2

let test2 = tsuru_no_ashi 2 = 4

let test2 = tsuru_no_ashi 3 = 6

(* *********************************** *)
(* 問題 4.7 *)
(* 鶴の数と亀の数の与えられたら、足の数を返す。*)
(* val tsurukame_no_ashi : int -> int -> int = <fun> *)
let tsurukame_no_ashi tn kn = tsuru_no_ashi tn + (kn * 4)

(* テスト *)
let test1 = tsurukame_no_ashi 0 0 = 0

let test1 = tsurukame_no_ashi 1 0 = 2

let test1 = tsurukame_no_ashi 0 1 = 4

let test1 = tsurukame_no_ashi 1 1 = 6

let test1 = tsurukame_no_ashi 2 2 = 12

let test1 = tsurukame_no_ashi 5 5 = 30

(* *********************************** *)
(* 問題 4.8 *)
(* 鶴xと亀yの数の合計aと、足の数の合計bを与えたら、　鶴xの数を返す。 *)
(* x+y=a, 2x+4y=b *)
(* 2x+2y=2a, 2x+4y=b 2x=2a-2y, 2x+4y=b 2a-2y+4y=b 2y=b-2a y=b/2-a yは亀の数

   x+y=a, 2x+4y=b 4x+4y=4a, 2x+4y=b 4y=4a-4x, 2x+4y=b 2x+4a-4x=b 4a-2x=b
   -2x=b-4a 2x=4a-b x=2a-b/2 xは鶴の数 *)
let tsurukame a b = (2 * a) - (b / 2) (*鶴の数を返す。 *)

let test1 = tsurukame 2 4 = 2

let test2 = tsurukame 2 6 = 1

let test3 = tsurukame 3 8 = 2

let test4 = tsurukame 4 10 = 3

(* *********************************** *)
(* 5.2 *)
(* 時間を受け取ったら、午前か午後を返す。*)
(* val jikan : int -> string *)
let jikan t = if t <= 12 then "午前" else "午後"

(* test *)
let test1 = jikan 1 = "午前"

let test2 = jikan 13 = "午後"

(* 誕生日(月と日)を受け取ったら、星座を返す。 *)
(* val seiza : int -> int -> string *)
let seiza y d =
  if (y = 3 && d >= 21) || (y = 4 && d <= 19) then "牡羊座" else "うお座"

let test1 = seiza 4 1 = "牡羊座"

let test1 = seiza 4 20 = "牡羊座"

(* 気温tから快適度の文字列を返す。 *)
(* val kaiteki : int -> bool *)
let kaiteki t = 15 <= t && t <= 25

(* val kion : int -> string *)
let kion t = if kaiteki t then "快適" else "普通"

let test1 = kion 7 = "普通"

let test2 = kion 15 = "快適"

let test3 = kion 20 = "快適"

let test4 = kion 25 = "快適"

let test5 = kion 28 = "普通"

(* *********************************** *)
(*5.4 *)
(* 目的: ax^2 +b2 +c = 0の実数の係数a,b,cが与えられたら、 判定式の値を返す。またa!=0 判定式は解の個数を求める式のこと。 *)
(* val hanteisiki : float -> float -> float -> float *)
let d a b c = (b *. b) -. (4.0 *. a *. c)

let hanteisiki a b c = d a b c

let test1 = hanteisiki 4.0 4.0 1.0 = 0.

let test2 = hanteisiki 1.0 3.0 1.0 = 5.0

let test3 = hanteisiki 3.0 2.0 10.0 = -116.

(* *********************************** *)
(* 5.5 *)
let hanbettsushiki a b c =
  let d = hanteisiki a b c in
  if d < 0. then 0 else if d = 0. then 1 else 2

let test1 = hanbettsushiki 4.0 4.0 1.0 = 1

let test2 = hanbettsushiki 1.0 3.0 1.0 = 2

let test3 = hanbettsushiki 3.0 2.0 10.0 = 0

(* *********************************** *)
(* 5.6 *)
let kyosuukai a b c = hanbettsushiki a b c = 0

let test1 = kyosuukai 4.0 4.0 1.0 = false

let test2 = kyosuukai 1.0 3.0 1.0 = false

let test3 = kyosuukai 3.0 2.0 10.0 = true

(* *********************************** *)
(* 5.7 *)
let bmi m kg = kg /. (m *. m)

let taikei m kg =
  let valbmi = bmi m kg in
  if valbmi < 18.5 then "やせ" else if valbmi < 25.0 then "標準" else "肥満"

let test1 = taikei 1.6 45. = "やせ"

let test1 = taikei 1.6 55. = "標準"

let test1 = taikei 1.6 77. = "肥満"

(* *********************************** *)
(* 6.1 *)
let square x = x * x

(* let test1 = square 3. *)
let test1 = square 3

let pi = 3.1415

let circle r = 2. *. pi *. r

let test1 = circle 2.

(* *********************************** *)
(* 6.3 *)
let tv bangumi youbi = bangumi ^ "は" ^ youbi ^ "に放送です。"

let test1 = tv "ニュース" "月"

(* *********************************** *)
(* 7.1 *)
(* 目的:5教科の点数を与えられたら その合計と平均を組で返す。 *)

let goukei_to_heikin a b c d e = (a + b + c + d + e, (a + b + c + d + e) / 5)

let test1 = goukei_to_heikin 0 0 0 0 0 = (0, 0)

let test1 = goukei_to_heikin 40 60 40 60 50 = (250, 50)

let test1 = goukei_to_heikin 100 100 100 100 100 = (500, 100)

(* *********************************** *)
(* 7.2 *)
(* 目的:名前と成績の組を受け取ったら、「xxの評価はyです」という文字列を返す。 *)
let seiseki student =
  match student with
  | name, score -> name ^ "の成績は" ^ string_of_int score ^ "です。"

let test1 = seiseki ("oki", 89) = "okiの成績は89です。"

let test1 = seiseki ("田中", 53) = "田中の成績は53です。"

let test1 = seiseki ("😍", 53) = "😍の成績は53です。"

(* *********************************** *)
(* 7.3 *)
let taisho point =
  match point with
  | x, y -> (x, -y)

let test1 = taisho (1, 1) = (1, -1)

let test1 = taisho (8, 2) = (8, -2)

let chuten p1 p2 =
  match (p1, p2) with
  | (x1, y1), (x2, y2) -> ((x1 + x2) / 2, (y1 + y2) / 2)

let test1 = chuten (1, 1) (-1, -1) = (0, 0)

let test2 = chuten (3, 1) (-3, 3) = (0, 2)

let chuten2 p1 p2 =
  match p1 with
  | x1, y1 -> (
    match p2 with
    | x2, y2 -> (x1 + x2, y1 + y2))

(* *********************************** *)
(* 8.1 *)
type student_t =
  { name : string
  ; score : int
  ; rank : string
  }

type book_t =
  { title : string
  ; author : string
  ; publisher : string
  ; price : int
  ; isbn10 : int
  }

let book1 =
  { title = "プログラミングの基礎"
  ; author = "浅井 健一"
  ; publisher = "サイエンス社"
  ; price = 2530
  ; isbn10 = 4781911609
  }

(* *********************************** *)
(* 8.2 *)
type okozukai_t =
  { name : string
  ; price : int
  ; shop : string
  ; date : int
  }

let okozukai1 = { name = "oki"; price = 100; shop = "マルエツ"; date = 1 }

let okozukai2 = { name = "oki"; price = 100; shop = "マルエツ"; date = 1 }

let okozukai3 = { name = "oki"; price = 100; shop = "マルエツ"; date = 1 }

let okozukai4 = { name = "oki"; price = 100; shop = "マルエツ"; date = 1 }

(* *********************************** *)
(* 問題8.3 *)
type person_t =
  { name : string
  ; height : float
  ; weight : float
  ; birthday : string
  ; blood : string
  }

let person1 =
  { name = "asai"
  ; height = 160.
  ; weight = 75.
  ; birthday = "19800505"
  ; blood = "B"
  }

let person2 =
  { name = "yamada"
  ; height = 160.
  ; weight = 75.
  ; birthday = "20111212"
  ; blood = "AB"
  }

let person3 =
  { name = "tanaka"
  ; height = 160.
  ; weight = 75.
  ; birthday = "19800305"
  ; blood = "O"
  }

let person4 =
  { name = "sato"
  ; height = 180.
  ; weight = 55.
  ; birthday = "19301021"
  ; blood = "A"
  }

(* デザインレシピ*)
(* 学生一人分の型 *)
type gakusei_t =
  { namae : string
  ; tensuu : int
  ; seiseki : string
  }

(* 目的: 学生データを受け取ったら成績のついたデータを返す。*)
(* val hyouka : gakusei_t -> gakusei_t = <fun> *)
let hyouka gakusei =
  match gakusei with
  | { namae = n; tensuu = t; seiseki = s } as r when t >= 80 ->
    { r with seiseki = "A" }
  | { namae = n; tensuu = t; seiseki = s } as r when t >= 70 ->
    { r with seiseki = "B" }
  | { namae = n; tensuu = t; seiseki = s } as r when t >= 60 ->
    { r with seiseki = "C" }
  | { namae = n; tensuu = t; seiseki = s } as r -> { r with seiseki = "D" }

let test1 =
  hyouka { namae = "asai"; tensuu = 90; seiseki = "" }
  = { namae = "asai"; tensuu = 90; seiseki = "A" }

let test1 =
  hyouka { namae = "asai"; tensuu = 70; seiseki = "" }
  = { namae = "asai"; tensuu = 70; seiseki = "B" }

(* *********************************** *)
(* 8.4 *)
let ketsueki_hyouji person =
  match person with
  | { name = n; blood = b } -> n ^ "の血液型は" ^ b ^ "です。"

let test1 = ketsueki_hyouji person1 = "asaiの血液型はBです。"

let test1 = ketsueki_hyouji person1 = "asaiの血液型はAです。"

(* *********************************** *)
(* 8.5 *)
(* metro.mlで定義したのでコメントアウト type ekimei_t = { kanji : string ; kana : string ;
   romaji : string ; shozoku : string } *)
let hyoji eki =
  match eki with
  | { kanji; kana; romaji; shozoku } ->
    shozoku ^ "," ^ " " ^ kanji ^ "(" ^ kana ^ ")"

let myogadani =
  { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸の内線" }

let test1 = hyoji myogadani = "丸の内線, 茗荷谷(みょうがだに)"

let m = hyoji myogadani

(* *********************************** *)
(* 8.7 matro.mlで定義したのでコメントアウト type ekikan_t = { kiten : string ; (* 起点の駅名 *)
   shuten : string ; (* 終点の駅名 *) keiyu : string ; kyori : float ; (* 2駅間の距離(km)
   *) jikan : int (* 所要時間 *) } *)
(* *********************************** *)
(* 9.1 *)
let seasons = [ "春"; "夏"; "秋"; "冬" ]

(* 9.2 *)
let persons = [ person1; person2; person3 ]

(* *********************************** *)
(*             要素::リスト              *)

(* 目的: 入力のリストに0が含まれるかを真偽値で返す。 *)
(* contain_zero : int list -> bool *)
let rec contain_zero ls =
  match ls with
  | [] -> false
  | first :: rest -> first = 0 || contain_zero rest

(* テスト *)
let test1 = contain_zero [] = false

let test2 = contain_zero [ 0 ] = true

let test3 = contain_zero [ 0; 1; 2; 3; 4; 5 ] = true

let test4 = contain_zero [ 1; 2; 3; 4; 5 ] = false

let test5 = contain_zero [ 1; 2; 3; 4; 5; 0 ] = true

let rec has_number n lst =
  match lst with
  | [] -> false
  | first :: rest -> n = first || has_number n rest

let t = [ 1; 2; 3; 4; 5 ]

let test1 = has_number 1 [ 1; 2; 3; 4 ] = true

let test2 = has_number 0 [ 1; 2; 3; 4 ] = false

let test2 = has_number 0 [ 1; 2; 3; 4 ] = true

let rec filter f = function
  | [] -> []
  | hd :: tl -> if f hd then hd :: filter f tl else filter f tl

let rect filter2 f = function
  | [] -> []
  | h :: t -> if f h then h :: filter2 f t else filter f t

let even = filter (fun n -> n mod 2 = 0)

let rec sum = function
  | [] -> 0
  | hd :: tl -> hd + sum tl

let test1 = sum [ 1; 2; 3; 4; 5 ] = 15

let test2 = sum [] = 0

(* *********************************** *)
(* 9.4 *)
let rec length = function
  | [] -> 0
  | hd :: tl -> 1 + length tl

let test1 = length [ 1; 2; 3; 4; 5 ] = 5

let test2 = length [] = 0

let test3 = length [ 1; 2; 3; 4; 5; 6; 7; 8 ] = 8

(* 9.5 *)
let rec even = function
  | [] -> []
  | hd :: tl -> if hd mod 2 = 0 then hd :: even tl else even tl
(* let test1 = (even [2;1;6;4;7]) = [2;6;4]; *)

(* 9.6 let rec concatx = function | [] -> "" | hd::tl -> hd ^ concatx tl *)

let rec concat = function
  | [] -> ""
  | hd :: tl -> hd ^ concat tl

let test1 = concat seasons = "春夏秋冬"

(* 学生データのリストを受け取ったら、その中に成績Aの人が何人いるか *)
let rec count_a = function
  | [] -> 0
  | first :: rest -> (if first.seiseki = "A" then 1 else 0) + count_a rest

let gakusei_list =
  [ { namae = "asai"; tensuu = 100; seiseki = "A" }
  ; { namae = "azai"; tensuu = 90; seiseki = "A" }
  ; { namae = "oda"; tensuu = 80; seiseki = "B" }
  ; { namae = "toyotomi"; tensuu = 70; seiseki = "B" }
  ; { namae = "akechi"; tensuu = 60; seiseki = "C" }
  ; { namae = "mouri"; tensuu = 50; seiseki = "C" }
  ; { namae = "houjou"; tensuu = 40; seiseki = "D" }
  ; { namae = "takeda"; tensuu = 35; seiseki = "D" }
  ; { namae = "uesugi"; tensuu = 30; seiseki = "E" }
  ; { namae = "imagawa"; tensuu = 20; seiseki = "E" }
  ; { namae = "shimazu"; tensuu = 10; seiseki = "E" }
  ; { namae = "saitou"; tensuu = 0; seiseki = "E" }
  ]

let test1 = count_a gakusei_list = 2

(* 9.7 *)
let persons =
  [ person1
  ; person2
  ; person3
  ; { name = "sato"
    ; height = 160.
    ; weight = 75.
    ; birthday = "19800304"
    ; blood = "A"
    }
  ; { name = "fukuda"
    ; height = 160.
    ; weight = 75.
    ; birthday = "19800304"
    ; blood = "A"
    }
  ]

let rec count_ketsueki_a lst =
  match lst with
  | [] -> 0
  | first :: rest ->
    (if first.blood = "A" then 1 else 0) + count_ketsueki_a rest

let test1 = count_ketsueki_a persons = 2

(* 9.8 *)
let rec filter f lst =
  match lst with
  | [] -> []
  | first :: rest -> if f first then first :: filter f rest else filter f rest

let otomeza = filter (fun n -> n.birthday = "19800304")

let test1 = otomeza persons

(* *********************************** *)
(* 10.1 *)
(* 接頭語のリストを受け取ったら、各接頭語の先頭にもう一つ要素を付け加える。*)
let rec add_to_each n lst =
  match lst with
  | [] -> []
  | first :: rest -> (n :: first) :: add_to_each n rest

let test1 = add_to_each 1 [] = []

let test2 = add_to_each 1 [ [ 2 ] ] = [ [ 1; 2 ] ]

let test3 = add_to_each 1 [ [ 2 ]; [ 2; 3 ] ] = [ [ 1; 2 ]; [ 1; 2; 3 ] ]

let test4 =
  add_to_each 1 [ [ 2 ]; [ 2; 3 ]; [ 2; 3; 4 ] ]
  = [ [ 1; 2 ]; [ 1; 2; 3 ]; [ 1; 2; 3; 4 ] ]

let rec prefix lst =
  match lst with
  | [] -> []
  | first :: rest -> [ first ] :: add_to_each first (prefix rest)

let test5 = prefix [] = []

let test6 = prefix [ 1 ] = [ [ 1 ] ]

let test7 = prefix [ 1; 2 ] = [ [ 1 ]; [ 1; 2 ] ]

let test7 = prefix [ 1; 2 ]

let test71 = prefix [ 1; 2; 3 ] = [ [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ] ]

let test8 =
  prefix [ 1; 2; 3; 4 ] = [ [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ]; [ 1; 2; 3; 4 ] ]

(* まずはこれが解けるか?1234567890S86I1l [1;2;3] -> [1;1+2;1+2+3] *)
let rec sum lst =
  match lst with
  | [] -> 0
  | hd :: tl -> hd + sum tl

let test1 = sum [ 1; 2 ] = 3

let test1 = sum [ 1; 2; 3 ] = 6

let rec sumx acc lst =
  match lst with
  | [] -> []
  | hd :: tl -> (hd + acc) :: sumx (hd + acc) tl

let test1 = sumx 0 [ 1; 2; 3 ] = [ 1; 3; 6 ]

let test2 = sumx 0 [ 1; 2 ] = [ 1; 3 ]

(* let rec sumx2 acc lst = match lst with [] -> [] | hd::tl -> (hd::acc)::sumx2
   (hd::acc) tl

   let test1 = sumx2 [] [1;2;3] = [1;3;6] *)

(* 数列アルゴリズム *)
(* 10.1 *)
let rec insert lst n =
  match lst with
  | [] -> [ n ]
  | h :: t -> if h < n then h :: insert t n else n :: lst

let test1 = insert [] 5

let test2 = insert [ 3 ] 5

let test3 = insert [ 1; 3; 4; 7; 8 ] 5

(* 10.2 *)
let rec ins_sort lst =
  match lst with
  | [] -> []
  | h :: t -> insert (ins_sort t) h

let test1 = ins_sort [ 3 ]

let test1 = ins_sort [ 3 ] = [ 3 ]

let test2 = ins_sort [ 5; 3 ]

let test2 = ins_sort [ 5; 3 ] = [ 3; 5 ]

let test3 = ins_sort [ 4; 5; 3 ]

let test3 = ins_sort [ 4; 5; 3 ] = [ 3; 4; 5 ]

let test4 = ins_sort [ 5; 3; 8; 1; 7; 4 ] = [ 1; 3; 4; 5; 7; 8 ]

(* 10.3 *)
let glist =
  [ { namae = "asai"; tensuu = 100; seiseki = "A" }
  ; { namae = "oda"; tensuu = 80; seiseki = "B" }
  ; { namae = "azai"; tensuu = 90; seiseki = "A" }
  ; { namae = "takeda"; tensuu = 90; seiseki = "A" }
  ]

let glist_sort =
  [ { namae = "oda"; tensuu = 80; seiseki = "B" }
  ; { namae = "azai"; tensuu = 90; seiseki = "A" }
  ; { namae = "takeda"; tensuu = 90; seiseki = "A" }
  ; { namae = "asai"; tensuu = 100; seiseki = "A" }
  ]

let rec g_insert lst gakusei =
  match lst with
  | [] -> [ gakusei ]
  | h :: t ->
    if h.tensuu < gakusei.tensuu then h :: g_insert t gakusei
    else gakusei :: h :: t

let test1 =
  g_insert [] { namae = "toyotomi"; tensuu = 95; seiseki = "A" }
  = [ { namae = "toyotomi"; tensuu = 95; seiseki = "A" } ]

let test2 =
  g_insert glist_sort { namae = "toyotomi"; tensuu = 95; seiseki = "A" }

let rec gakusei_sort lst =
  match lst with
  | [] -> []
  | h :: t -> g_insert (gakusei_sort t) h

let test1 = gakusei_sort glist

(* 10.4 *)
let rec person_insert lst p =
  match lst with
  | [] -> [ p ]
  | h :: t -> if h.name < p.name then h :: person_insert t p else p :: lst

let p1 =
  { name = "asai"
  ; height = 160.
  ; weight = 75.
  ; birthday = "19800505"
  ; blood = "B"
  }

let test1 = person_insert [] p1 = [ p1 ]

let test2 = person_insert [ person2 ] p1 = [ p1; person2 ]

let rec person_sort lst =
  match lst with
  | [] -> []
  | h :: t -> person_insert (person_sort t) h

let test1 = person_sort persons

(* 10.2 リストの中野最小値を求める関数 *)
let rec minimum lst =
  match lst with
  | [] -> max_int
  | h :: t -> if h < minimum t then h else minimum t

let test1 = minimum [ 3 ] = 3

let test2 = minimum [ 1; 2 ] = 1

let test3 = minimum [ 3; 2 ] = 2

let test4 = minimum [ 3; 2; 6; 4; 1; 8 ] = 1

let rec maximum = function
  | [] -> min_int
  | h :: t -> if h > maximum t then h else maximum t

let test1 = maximum [ 3 ] = 3

let test2 = maximum [ 1; 2 ] = 2

let test3 = maximum [ 3; 2 ] = 3

let test4 = maximum [ 3; 2; 6; 4; 1; 8 ] = 8

(* 10.5 *)
(* 10.6 let x in を使う。 *)
let rec gakusei_max = function
  | [] -> { namae = ""; tensuu = 0; seiseki = "" }
  | h :: t ->
    let rest = gakusei_max t in
    if h.tensuu > rest.tensuu then h else rest

let test1 = gakusei_max [] = { namae = ""; tensuu = 0; seiseki = "" }

let test1 = gakusei_max [ { namae = "oki"; tensuu = 90; seiseki = "A" } ]

let test2 =
  gakusei_max
    [ { namae = "oki"; tensuu = 90; seiseki = "A" }
    ; { namae = "tana"; tensuu = 91; seiseki = "A" }
    ]

let test3 =
  gakusei_max
    [ { namae = "oki"; tensuu = 10; seiseki = "A" }
    ; { namae = "tana"; tensuu = 21; seiseki = "A" }
    ; { namae = "abe"; tensuu = 81; seiseki = "A" }
    ]

(* 10.4 パターンマッチ付き局所変数定義 *)
(* 学生リストのうち各成績の人数を集計する *)
let rec shukei lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | { namae = n; tensuu = t; seiseki = s } :: rest ->
    let a, b, c, d = shukei rest in
    if s = "A" then (a + 1, b, c, d)
    else if s = "B" then (a, b + 1, c, d)
    else if s = "C" then (a, b, c + 1, d)
    else (a, b, c, d + 1)

(* 10.7 person_t型のリストを受け取ったら、各血液型を集計した人数を組で返す。 *)
let rec ketsueki_shukei lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | first :: rest ->
    let a, b, o, ab = ketsueki_shukei rest in
    if first.blood = "A" then (a + 1, b, o, ab)
    else if first.blood = "B" then (a, b + 1, o, ab)
    else if first.blood = "AB" then (a, b, o, ab + 1)
    else (a, b, o + 1, ab)

let test1 = ketsueki_shukei [] = (0, 0, 0, 0)

let test2 = ketsueki_shukei [ person1 ] = (0, 1, 0, 0)

let test2 = ketsueki_shukei [ person1; person2 ] = (0, 1, 0, 1)

(* 10.8 *)
let rec max' lst =
  match lst with
  | [] -> min_int
  | h :: t -> if h > max' t then h else max' t

let test1 = max' [] = min_int

let test2 = max' [ 3 ] = 3

let test3 = max' [ 3; 5 ] = 5

let test4 = max' [ 5; 3 ] = 5

let test5 = max' [ 5; 3; 8 ] = 8

let test5 = max' [ 5; 3; 8; 2 ] = 8

let saita_ketsueki lst =
  match lst with
  | [] -> ""
  | h :: t ->
    let a, b, o, ab = ketsueki_shukei lst in
    (* count by blood *)
    let blst = [ a; b; o; ab ] in
    let bmax = max' blst in
    (* get most count in blood *)
    if bmax = a then "A"
    else if bmax = b then "B"
    else if bmax = o then "O"
    else "AB"

let test1 = saita_ketsueki [ person1 ]

(* 10.5 ふたつのリストを結合する関数 *)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | first :: rest -> first :: append rest lst2

let test1 = append [] [] = []

let test2 = append [] [ 1; 2 ] = [ 1; 2 ]

let test3 = append [ 1; 2 ] [] = [ 1; 2 ]

let test4 = append [ 1; 2 ] [ 3; 4 ] = [ 1; 2; 3; 4 ]

let test5 =
  append [ "a"; "b"; "c"; "d"; "e" ] [ "f"; "g" ]
  = [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ]

(* 10.6 ふたつの昇順に並んだリストをマージする関数 *)
(* マージソートを使うときに使われる関数 *)

(* 目的：昇順に並んでいるlst1とlst2をマージする *)
(* val merge : int list -> int list -> int list *)

let rec merge lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> []
  | [], first2 :: rest2 -> first2 :: merge lst1 rest2 (* merge lst1 rest2 *)
  | first1 :: rest1, [] -> first1 :: merge rest1 lst2 (* merge rest1 lst2 *)
  | first1 :: rest1, first2 :: rest2 ->
    (* マージを実装するうえでの注意点は、両方のリスト(lst1,lst2)が残っている時、大小を比較し
       その１つだけを先頭に加えて、merge関数を呼ぶ。以下のように２つの要素(first1,first2)を この関数の呼び出しで処理しない。
       first1::first2::merge rest1 rest2 *)
    if first1 < first2 then first1 :: merge rest1 lst2
      (* ここではlst2を短くする再帰, lst1を短くする再帰, 両方を短くする再帰,が考えられる。*)
      (* merge lst1 rest2, merge rest1 lst2, merge rest1 rest2 *)
    else first2 :: merge lst1 rest2

(* lst2を短くする再帰, lst1を短くする再帰, 両方を短くする再帰,が考えられる。*)
(* merge lst1 rest2, merge rest1 lst2 merge rest1 rest2 *)

let test1 = merge [] [] = []

let test2 = merge [] [ 1; 2 ] = [ 1; 2 ]

let test3 = merge [ 1; 2 ] [] = [ 1; 2 ]

let test4 = merge [ 1; 3 ] [ 2; 4 ] = [ 1; 2; 3; 4 ]

let test4 = merge [ 2; 4 ] [ 1; 3 ] = [ 1; 2; 3; 4 ]

let test4 = merge [ 1; 4 ] [ 1; 3 ] = [ 1; 1; 3; 4 ]

(* 10.9 *)
(* ふたつのリストを受け取ったら、 長さが同じかどうか判定する関数equal_length length関数は使わずに実装 *)
(* var equal_length : 'a list -> 'a list -> int *)
let rec equal_length lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | _ :: xs, _ :: ys -> equal_length xs ys

let test1 = equal_length [] [] = true

let test1 = equal_length [ 1 ] [] = false

let test1 = equal_length [] [ 1 ] = false

let test1 = equal_length [ 1 ] [ 1 ] = true

let test1 = equal_length [ 1; 2 ] [ 1 ] = false

let test1 = equal_length [ 2 ] [ 1; 2 ] = false

let test1 = equal_length [ 1; 2 ] [ 1; 2 ] = true

let test1 = equal_length [ 1; 2; 3 ] [ 1; 2 ] = false

let test1 = equal_length [ 1; 2 ] [ 1; 2; 3 ] = false

(* 10.7 駅名・駅間リストからの情報の取得 *)
(* メトロネットワーク最短経路問題で必要な関数の作成 1. 駅名リストからローマ字を漢字に変換する。 *)

(* 問題 10.10 🚃電車 *)
(* 9.9のglobal_ekimei_list metro.mlで定義 *)

(** ローマ字の駅名文字列と、ekimei_t型の駅名リストを受け取ったら、漢字を返す。*)
let rec romaji_to_kanji romaji = function
  | [] -> ""
  | h :: t -> if h.romaji = romaji then h.kanji else romaji_to_kanji romaji t

let test1 = romaji_to_kanji "myogadani" global_ekimei_list

let test2 = romaji_to_kanji "myogadani x" global_ekimei_list = ""

(* 問題10.11 🚃電車 *)
(* 漢字の駅名ふたつと駅間リスト(ekikan_t list)を受け取ったら、 駅間の距離を返す関数get_ekikan_kyori
   駅間リストには、global_ekikan_listを使う 駅がつながっていない場合は無限大infinityを返す。 *)

(* {kiten="代々木上原"; shuten="代々木公園"; keiyu="千代田線"; kyori=1.0; jikan=2}; *)
let rec get_ekikan_kyori eki1_name eki2_name lst =
  match lst with
  | [] -> infinity
  | h :: t ->
    if h.kiten = eki1_name && h.shuten = eki2_name then h.kyori
    else if h.kiten = eki2_name && h.shuten = eki1_name then h.kyori
    else get_ekikan_kyori eki1_name eki2_name t

let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2

let test2 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_list = 1.2

(* {kiten="浦安" ; shuten="葛西"; keiyu="東西線"; kyori=1.9 ; jikan=2}; *)

let test3 = get_ekikan_kyori "浦安" "葛西" global_ekikan_list = 1.9

let test4 = get_ekikan_kyori "葛西" "浦安" global_ekikan_list = 1.9

(* 問題10.12 🚃電車 *)
(* ローマ字の駅名文字列を２つ受け取ったら、
 * その距離を調べ、直接つながっている場合は文字列「A駅からB駅まではx kmです」を返す。
 * つながっていない場合は文字列「A駅とB駅はつながってません」を返す。
 * *)
let kyori_wo_hyoji a b =
  let ak = romaji_to_kanji a global_ekimei_list in
  let bk = romaji_to_kanji b global_ekimei_list in
  let kyori = get_ekikan_kyori ak bk global_ekikan_list in
  if kyori = infinity then ak ^ "駅と" ^ bk ^ "はつながっていません"
  else ak ^ "駅から" ^ bk ^ "駅までは" ^ string_of_float kyori ^ "です"

let test1xxxxxxxxxxxxxxx = kyori_wo_hyoji "myogadani" "shinotsuka"

let test2xxxxxxxxxxxxxxx = kyori_wo_hyoji "myogadani" "korakuen"

(* 11.2 階乗 *)
let rec factorial n = if n = 0 then 1 else n * factorial (n - 1)

let test1 = factorial 0 = 1

let test2 = factorial 1 = 1

let test3 = factorial 2 = 2

let test4 = factorial 3 = 6

let test5 = factorial 4 = 24

(* 11.3 べき乗を求める関数 *)
let rec power m n = if n = 0 then 1 else m * power m (n - 1)

let test1 = power 3 0 = 1

let test2 = power 3 1 = 3

let test3 = power 3 2 = 9

let test4 = power 3 3 = 27

(* 11.1 0から受け取った自然数までの二乗の和を求めるsum_of_square *)
let rec sum_of_square n =
  if n = 0 then 0 * 0 else sum_of_square (n - 1) + (n * n)

let test1 = sum_of_square 4 = 30 (* 0*0 +1*1 + 2*2 + 3*3 + 4*4 *)

let test2 = sum_of_square 0 = 0 (* 0*0 *)

let test3 = sum_of_square 1 = 1 (* 0*0 + 1 * 1 *)

let test4 = sum_of_square 2 = 5 (* 0*0 + 1 * 1 + 2 * 2 *)

let test5 = sum_of_square 3 = 14 (* 0*0 + 1 * 1 + 2 * 2 + 3 * 3 *)

(* 11.2 数列anの第n項を求める関数α *)
let rec a n = if n = 0 then 3 else (2 * a (n - 1)) - 1

let test1 = a 0 = 3

let test2 = a 1 = 5

let test3 = a 2 = 9

let test4 = a 3 = 17
(* a 3 = (2 * a 2) - 1 = a 2 = (2 * a 1) - 1 = (2 * 5) -1 = 9 a 1 = (2 * a 0) -
   1 = 5 a 0 = 3 = 3 *)

(* * 12章 ダイクストラのアルゴリズム *)
(* 12.1 🚃 *)
(* 8.7 で ekimei_t型定義 *)
(* 9.7でglobal_ekimei_listを整備 *)
(* metro.mlをダウンロードしてnkfでutf-8にして使う。 *)

(* グラフの頂点集合にはeki_t型を使う *)
type eki_t =
  { namae : string
  ; saitan_kyori : float
  ; temae_list : string list
  }

(* 12.2 🚃 type ekimei_t = { kanji : string; (* 駅名 *) kana : string; (* 読み *)
   romaji : string; (* ローマ字 *) shozoku : string; (* 所属線名 *) } *)
(* ekimei_t型リストからeki_t型リストを作成 *)
let rec make_eki_list lst =
  match lst with
  | [] -> []
  | h :: t ->
    { namae = h.kanji; saitan_kyori = infinity; temae_list = [] }
    :: make_eki_list t

let yy =
  { kanji = "代々木上原"
  ; kana = "よよぎうえはら"
  ; romaji = "yoyogiuehara"
  ; shozoku = "千代田線"
  }

let test1 = make_eki_list [] = []

let test2 =
  make_eki_list [ yy ]
  = [ { namae = yy.kanji; saitan_kyori = infinity; temae_list = [] } ]

let test3 = make_eki_list global_ekimei_list

(* 問題 12.3 🚃 *)
let eki_list = make_eki_list global_ekimei_list

(* eki_t型リストｔと起点（漢字）を受け取ったら 起点のみsaitan_kyori=0, temae_list=始点の駅名リスト *)
let rec shokika eki_list kiten_kanji =
  match eki_list with
  | [] -> []
  | h :: t ->
    if h.namae = kiten_kanji then
      { h with saitan_kyori = 0.; temae_list = [ h.namae ] }
      :: shokika t kiten_kanji
    else h :: shokika t kiten_kanji

let test1 =
  List.filter (fun x -> x.namae = "代々木上原") (shokika eki_list "代々木上原")
  = [ { namae = "代々木上原"; saitan_kyori = 0.; temae_list = [ "代々木上原" ] } ]

(* 問題 12.4 🚃 global_ekimei_listから重複を取り除く *)

let insert lst n =
  match lst with
  | [] -> [ n ]
  | h :: t -> if h < n then h :: insert t n else n :: lst

let test1 = insert [] 3 = [ 3 ]

let test2 = insert [ 5 ] 3 = [ 3; 5 ]

let test3 = insert [ 3 ] 5 = [ 3; 5 ]

let test4 = insert [ 1; 3 ] 5 = [ 1; 3; 5 ]

let test5 = insert [ 1; 2; 4; 7; 8 ] 5 = [ 1; 2; 4; 5; 7; 8 ]

let ins_sort = function
  | [] -> []
  | h :: t -> insert (ins_sort t) h

let test1 = ins_sort [] = []

let test1 = ins_sort [ 1 ] = [ 1 ]

let test1 = ins_sort [ 3; 1 ] = [ 1; 3 ]

let test1 = ins_sort [ 3; 3; 1; 5 ] = [ 1; 3; 3; 5 ]

let test1 = ins_sort [ 3; 3; 1; 5 ]

let test1 = ins_sort [ 5; 3; 3; 1; 1 ]

let test1 = ins_sort [ 5; 3; 3; 1; 1; 5 ]

(** ekimei_t型のリストglobal_ekimei_listを受け取り * ひらがなで整列し、 * 重複を取り除いたekimei_t型リストを返す。 *)
let rec seiretsu_insert lst n =
  match lst with
  | [] -> [ n ]
  | h :: t ->
    if h.kana = n.kana then seiretsu_insert t n
    else if h.kana < n.kana then h :: seiretsu_insert t n
    else n :: lst

(** ekimei_t型のリストを受け取り、ひらがなで整列し、重複を取り除いたekimei_t型リストを返す。 *)
let rec seiretsu = function
  | [] -> []
  | h :: t -> seiretsu_insert (seiretsu t) h

let ekimei_list =
  [ { kanji = "代々木上原"
    ; kana = "よよぎうえはら"
    ; romaji = "yoyogiuehara"
    ; shozoku = "千代田線"
    }
  ; { kanji = "代々木公園"
    ; kana = "よよぎこうえん"
    ; romaji = "yoyogikouen"
    ; shozoku = "千代田線"
    }
  ; { kanji = "明治神宮前"
    ; kana = "めいじじんぐうまえ"
    ; romaji = "meijijinguumae"
    ; shozoku = "千代田線"
    }
  ; { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" }
  ; { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "有楽町線" }
  ]

let expected_list =
  [ { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" }
  ; { kanji = "明治神宮前"
    ; kana = "めいじじんぐうまえ"
    ; romaji = "meijijinguumae"
    ; shozoku = "千代田線"
    }
  ; { kanji = "代々木上原"
    ; kana = "よよぎうえはら"
    ; romaji = "yoyogiuehara"
    ; shozoku = "千代田線"
    }
  ; { kanji = "代々木公園"
    ; kana = "よよぎこうえん"
    ; romaji = "yoyogikouen"
    ; shozoku = "千代田線"
    }
  ]

let test1 = seiretsu ekimei_list = expected_list

let test1 = seiretsu ekimei_list

(* int型で昇順の重複したデータのあるリストを受け取ったら、 重複がないデータを返す。*)
let rec unique_insert lst n =
  match lst with
  | [] -> [ n ]
  | h :: t ->
    if h = n then unique_insert t n (* if the list contain n, skip adding it. *)
    else if h < n then h :: unique_insert t n
    else n :: lst

let test1 = unique_insert [] 5 = [ 5 ]

let test1 = unique_insert [ 3 ] 5 = [ 3; 5 ]

let test1 = unique_insert [ 5 ] 3 = [ 3; 5 ]

let test1 = unique_insert [ 1; 5 ] 3 = [ 1; 3; 5 ]

let test1 = unique_insert [ 1; 2; 5 ] 3 = [ 1; 2; 3; 5 ]

let test1 = unique_insert [ 1; 3; 4; 7; 8 ] 5 = [ 1; 3; 4; 5; 7; 8 ]

let test1 = unique_insert [ 1; 3; 4; 7; 8 ] 5 = [ 1; 3; 4; 5; 7; 8 ]

let test1 = unique_insert [ 1; 3; 4; 7; 8 ] 1 = [ 1; 3; 4; 7; 8 ]

let rec unique_sort_insert = function
  | [] -> []
  | h :: t -> unique_insert (unique_sort_insert t) h

let test1 = unique_sort_insert [ 1; 3; 4; 7; 8 ]

let test1 = unique_sort_insert [ 5 ] = [ 5 ]

let test1 = unique_sort_insert [ 5; 3 ] = [ 3; 5 ]

let test1 = unique_sort_insert [ 1; 5; 3 ] = [ 1; 3; 5 ]

let test1 = unique_sort_insert [ 1; 5; 3; 8; 1; 7; 4 ] = [ 1; 3; 4; 5; 7; 8 ]

let test1 =
  unique_sort_insert [ 1; 5; 5; 3; 5; 8; 1; 5; 7; 4 ] = [ 1; 3; 4; 5; 7; 8 ]

(*
 * 13章　一般化と高階関数
 *)
(* データの一般化 *)
let rec count_A = function
  | [] -> 0
  | h :: t -> if h.seiseki = "A" then 1 + count_A t else count_A t

let rec count_A = function
  | [] -> 0
  | h :: t when h.seiseki = "A" -> 1 + count_A t
  | h :: t -> count_A t

let test1 = count_A [ { namae = "oki"; tensuu = 90; seiseki = "A" } ] = 1

let test1 =
  count_A
    [ { namae = "oki"; tensuu = 90; seiseki = "A" }
    ; { namae = "tanaka"; tensuu = 90; seiseki = "A" }
    ]
  = 2

let rec count_B = function
  | [] -> 0
  | h :: t -> if h.seiseki = "B" then 1 + count_B t else count_B t

let test1 = count_B [ { namae = "oki"; tensuu = 90; seiseki = "B" } ] = 1

let rec count lst seiseki =
  match lst with
  | [] -> 0
  | h :: t ->
    if h.seiseki = seiseki then 1 + count t seiseki else count t seiseki

let test1 = count [ { namae = "oki"; tensuu = 90; seiseki = "B" } ] "B" = 1

(* 13.1 *)
let rec count_ketsueki p_lst blood =
  match p_lst with
  | [] -> 0
  | h :: t ->
    if h.blood = blood then 1 + count_ketsueki t blood
    else count_ketsueki t blood

let test1 = count_ketsueki persons "A" = 2

let test2 = count_ketsueki persons "B" = 1

(* 13.2 関数の一般化とmap *)
let rec map_sqrt = function
  | [] -> []
  | h :: t -> sqrt h :: map_sqrt t

let test1 = map_sqrt [ 4.; 9.; 16. ] = [ 2.; 3.; 4. ]

let rec map_hyouka = function
  | [] -> []
  | h :: t -> hyouka h :: map_hyouka t

let test1 =
  map_hyouka [ { namae = "asai"; tensuu = 90; seiseki = "" } ]
  = [ { namae = "asai"; tensuu = 90; seiseki = "A" } ]

let test2 =
  map_hyouka [ { namae = "asai"; tensuu = 90; seiseki = "" } ]
  != [ { namae = "asai"; tensuu = 90; seiseki = "B" } ]

let rec map f lst =
  match lst with
  | [] -> []
  | h :: t -> f h :: map f t

let test1 = map sqrt [ 4.; 9.; 16. ] = [ 2.; 3.; 4. ]

let tes1 =
  map hyouka [ { namae = "asai"; tensuu = 90; seiseki = "" } ]
  = [ { namae = "asai"; tensuu = 90; seiseki = "A" } ]

let map_sqrt = map sqrt

let test1 = map_sqrt [ 4.; 9.; 16. ] = [ 2.; 3.; 4. ]

let map_hyouka' = map hyouka

let test1 =
  map_hyouka' [ { namae = "asai"; tensuu = 90; seiseki = "" } ]
  = [ { namae = "asai"; tensuu = 90; seiseki = "A" } ]

let test2 =
  map_hyouka' [ { namae = "asai"; tensuu = 90; seiseki = "" } ]
  != [ { namae = "asai"; tensuu = 90; seiseki = "B" } ]

(* 13.2 *)
let pname p = p.name

let person_name lst = map pname lst

let person_name = map (fun person -> person.name)

let test1 = person_name persons

let person_name2 = List.map (fun p -> p.name)

let test1 = person_name2 persons

(* 13.3 *)
let rec length = function
  | [] -> 0
  | h :: t -> 1 + length t

let test1 = length [ 1; 2; 3; 4; 5 ] = 5

(* 13.5 関数を返す関数 *)
(* 関数呼び出しは左結合 *)
let add3 n = 3 + n

let twice f =
  let g x = f (f x) in
  g

(* 13.3 *)
let p13_3_1 n = n

let p13_3_2 a b = a

let p13_3_3 a b = b

let p13_3_4 a f =
  let b = f a in
  b

let fx f1 f2 n = f2 (f1 n)

(* 13.4 *)
let compose f g n = f (g n)

let time2 n = n * 2

let add3 n = n + 3

let test = compose time2 add3 4 = 14

(* 13.5 *)
let twice f =
  let g x = f (f x) in
  g

let test1 = twice add3 7

let twice2 = twice twice

let test1 = twice2 (fun n -> n + 1) 5 = 9

(* 13.6 確定点に隣接する点の最短距離の更新 *)
(* ステップ４を作る。 *)

(* 直前に確定した駅 p:eki_tと未確定の駅 q:eki_tを受け取ったら、
 * つながっているかどうかを調べ、
 * つながっていなければ、そのままqを返す。
 * つながっていればqの最短距離と手前のリストを必要に応じて更新、 *)
let rec koushin1 p q =
  let kyori = get_ekikan_kyori p.namae q.namae global_ekikan_list in
  if kyori = infinity then q
  else if p.saitan_kyori +. kyori < q.saitan_kyori then
    { q with
      saitan_kyori = p.saitan_kyori +. kyori
    ; temae_list = q.namae :: p.temae_list
    }
  else q

let p1 = { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = [] }

let q1 = { namae = "新大塚"; saitan_kyori = 0.; temae_list = [] }

let test1 = koushin1 p1 q1

(* todo テストを追加 *)
(* 13.7 *)
let rec koushin p v = map (fun e -> koushin1 p e) v

(* todo テストを追加 *)

(*
 * 第14章 高階関数を使ったリスト操作
 *)

let rec filter_positive = function
  | [] -> []
  | h :: t -> if h > 0 then h :: filter_positive t else filter_positive t

let test1 = filter_positive [ -3; -2; -1; 0; 1; 2; 3; 4; 5 ] = [ 1; 2; 3; 4; 5 ]

let test2 = filter_positive [ -3; -2; -1; 0 ] = []

let test3 = filter_positive [ 3; 4; 5 ] = [ 3; 4; 5 ]

let rec filter_mod3_1 = function
  | [] -> []
  | h :: t -> if h mod 3 = 1 then h :: filter_mod3_1 t else filter_mod3_1 t

let test1 = filter_mod3_1 [ 1; 2; 3; 4; 5 ] = [ 1; 4 ]

let test1 = filter_mod3_1 [ 2; 3 ] = []

let rec filter f lst =
  match lst with
  | [] -> []
  | h :: t -> if f h then h :: filter f t else filter f t

let filter_positive' = filter (fun n -> n > 0)

let test1 =
  filter_positive' [ -3; -2; -1; 0; 1; 2; 3; 4; 5 ] = [ 1; 2; 3; 4; 5 ]

let test2 = filter_positive' [ -3; -2; -1; 0 ] = []

let test3 = filter_positive' [ 3; 4; 5 ] = [ 3; 4; 5 ]

let filter_mod3_1' = filter (fun n -> n mod 3 = 1)

let test1 = filter_mod3_1' [ 1; 2; 3; 4; 5 ] = [ 1; 4 ]

let test1 = filter_mod3_1' [ 2; 3 ] = []

(* 14.1 *)
let rec filter f lst =
  match lst with
  | [] -> []
  | h :: t -> if f h then h :: filter f t else filter f t

let even = filter (fun n -> n mod 2 = 0)

(* 14.2 *)
let rec length = function
  | [] -> 0
  | h :: t -> 1 + length t

(* gakusei_t *)
let count_A lst = length (filter (fun gt -> gt.seiseki = "A") lst)

let test1 = count_A [] = 0

let test2 = count_A gakusei_list = 2

let gakusei_list2 =
  [ { namae = "asai"; tensuu = 100; seiseki = "A" }
  ; { namae = "oda"; tensuu = 80; seiseki = "B" }
  ; { namae = "mouri"; tensuu = 50; seiseki = "C" }
  ; { namae = "houjou"; tensuu = 40; seiseki = "D" }
  ; { namae = "imagawa"; tensuu = 20; seiseki = "E" }
  ]

let test3 = count_A gakusei_list2 = 1

let rec fold_right f lst init =
  match lst with
  | [] -> init
  | h :: t -> f h (fold_right f t init)

let sum' lst = fold_right (fun n a -> n + a) lst 0

let test1 = sum' [ 1; 2; 3; 4; 5 ] = 15

(* sum' [1;2;3;4;5] fold_right (fun n a -> n + 1) [1;2;3;4;5] 0 1::[2;3;4;5] ->
   f 1 (fold_right f [2;3;4;5] 0) 2::[3;4;5] -> f 2 (fold_right f [3;4;5] 0)
   3::[4;5] -> f 3 (fold_right f [4;5] 0) 4::[5] -> f 4 (fold_right f [5] 0)
   5::[] -> f 5 (fold_right f [] 0) [] -> 0

   fold_rightの構造は、 1.一度リストをすべて展開する。つまり[1;2;3;4;5]とリストがあったら、 空[]になるまで広げる。
   2.一番右の要素から関数を適用してその結果をaccumulatorとして累積していく。 *)

let length' lst = fold_right (fun _ a -> 1 + a) lst 0

let test1 = length' [ 1; 2; 3; 4; 5 ] = 5

let cons h t = h :: t

let append' xs ys = fold_right cons xs ys

let test1 = append' [ 1; 2; 3 ] [ 4; 5; 6 ] = [ 1; 2; 3; 4; 5; 6 ]

let test1 = append' [ 4; 5; 6 ] [ 1; 2; 3 ] = [ 4; 5; 6; 1; 2; 3 ]

(* 14.3 *)
let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

let test1 = concat [ "h"; "e"; "l"; "l"; "o" ]

let cf c acc = c ^ acc

let concat' lst = fold_right cf lst ""

let test1 = concat' [ "w"; "o"; "r"; "l"; "d"; "😍" ]

let gakusei_sum lst = fold_right (fun g acc -> g.tensuu + acc) lst 0

let gakusei_list3 =
  [ { namae = "asai"; tensuu = 100; seiseki = "A" }
  ; { namae = "azai"; tensuu = 90; seiseki = "A" }
  ; { namae = "oda"; tensuu = 80; seiseki = "B" }
  ; { namae = "toyotomi"; tensuu = 70; seiseki = "B" }
  ; { namae = "akechi"; tensuu = 60; seiseki = "C" }
  ; { namae = "mouri"; tensuu = 50; seiseki = "C" }
  ; { namae = "houjou"; tensuu = 40; seiseki = "D" }
  ; { namae = "uesugi"; tensuu = 30; seiseki = "E" }
  ; { namae = "imagawa"; tensuu = 20; seiseki = "E" }
  ; { namae = "shimazu"; tensuu = 10; seiseki = "E" }
  ; { namae = "saitou"; tensuu = 0; seiseki = "E" }
  ]

let test1 = gakusei_sum gakusei_list3 = 550

let ( |> ) x f = fx

(* 14.5 *)
(* 14.1を無名関数と自作のfilterで書き直す *)
let filter f lst =
  match lst with
  | [] -> []
  | h :: t -> if f h then h :: filter f t else filter f t

let even = filter (fun n -> n mod 2 = 0)

let test1 = even [ 1; 2; 3; 4; 5 ] = [ 2; 4 ]

let test2 = even [ 1; 2 ] = [ 2 ]

let test3 = even [ 1 ] = []

let test4 = even [] = []

let test1 = List.filter (fun a -> a = 0) [ 1; 2; 3 ]

(* 14.2のcount_Aを無名関数を使って書き直す。 *)
let length lst = fold_right (fun _ sum -> 1 + sum) lst 0

let count_A lst = length (filter (fun g -> g.seiseki = "A") lst)

let test1 = count_A gakusei_list = 2

(* 14.3 concatを*)
let rec fold_right'' f lst init =
  match lst with
  | [] -> init
  | h :: t -> f h (fold_right'' f t init)

let concat' lst = fold_right'' (fun s acc -> s ^ acc) lst ""

let test1 = concat' [ "c"; "a"; "m"; "l" ] = "caml"

(* str_to_list *)

(* 14.8 *)
let test1 = List.map (fun n -> (n * n) - 1) [ 1; 2; 3; 4; 5 ]

(* 14.9 *)
let test1 = List.map (fun p -> p.name) persons

(* 14.11 *)
(* 問題12.2で作ったmake_eki_listと問題12.3で作ったshokika関数をmapと無名関数で作り直す。*)
(* 問題12.2で作ったmake_eki_listをmapと無名関数で実装*)

let make_eki_list' l =
  List.map
    (fun eki -> { namae = eki.kanji; saitan_kyori = infinity; temae_list = [] })
    l

let test1 = make_eki_list' [] = []

let test2 =
  make_eki_list' [ yy ]
  = [ { namae = yy.kanji; saitan_kyori = infinity; temae_list = [] } ]

(* TODO compare from file let test3 = make_eki_list' global_ekimei_list *)

(* 問題12.3で作ったshokikaをmapと無名関数で実装*)
let shokika' l kiten_kanji =
  List.map
    (fun a ->
      if a.namae = kiten_kanji then
        { namae = a.namae; saitan_kyori = 0.; temae_list = [ a.namae ] }
      else a)
    l

let test1 =
  let actual_lst = shokika' eki_list "代々木上原" in
  List.filter (fun a -> a.namae = "代々木上原") actual_lst
  = [ { namae = "代々木上原"; saitan_kyori = 0.; temae_list = [ "代々木上原" ] } ]

(* 問題14.12 *)
(* make_eki_listとshokikaを１つの関数で作る *)

(** 駅リストと起点の漢字名とを受け取り、駅リストを初期化する。*)
let make_initial_eki_list l kiten_kanji =
  List.map
    (fun a ->
      if a.kanji = kiten_kanji then
        { namae = a.kanji; saitan_kyori = 0.; temae_list = [ a.kanji ] }
      else { namae = a.kanji; saitan_kyori = infinity; temae_list = [] })
    l

let test1 =
  let l = make_initial_eki_list global_ekimei_list "代々木上原" in
  List.filter (fun a -> a.namae = "代々木上原") l
  = [ { namae = "代々木上原"; saitan_kyori = 0.; temae_list = [ "代々木上原" ] } ]

let test1 =
  let l = make_initial_eki_list global_ekimei_list "代々木上原" in
  List.filter (fun a -> a.namae = "南千住") l
  = [ { namae = "南千住"; saitan_kyori = infinity; temae_list = [] } ]

(* 問題14.13 *)

(* 14.5 infix関数とprefix関数 *)
let sum lst = fold_right ( + ) lst 0

let test1 = sum [ 1; 2; 3; 4; 5 ] = 15

(* 問題14.14 *)
let concat lst = fold_right ( ^ ) lst ""

let test1 = concat [ "a"; "b"; "c" ] = "abc"

(* 14.6 完全数を求める関数 *)
(* 完全数 自分自身を除く約数(divisor)の和がその和自身になる和。 1. nから1までのリストを作るenumerate関数を作る *)
let rec enumerate n = if n = 0 then [] else n :: enumerate (n - 1)

let test1 = enumerate 5 = [ 5; 4; 3; 2; 1 ]

let test1 = enumerate 0 = []

let divisor n = filter (fun x -> n mod x = 0) (enumerate n)

let test1 = divisor 6 = [ 6; 3; 2; 1 ]

let test1 = divisor 24 = [ 24; 12; 8; 6; 4; 3; 2; 1 ]

(* 遅い *)
let perfect m =
  (* 約数nでn自身を除く約数の和がnと等しい
   * fold_right f   lst init
   * fold_right (+) [6;3;2;1] (0 - 6)
   * (0 - 6)をすることで、約数6自身をリストから削除したことと同じになる。
   * fold_right (+) [3;2;1]   0 
   *)
  filter (fun n -> fold_right ( + ) (divisor n) 0 - n = n) (enumerate m)

let test1 = perfect 6 = [ 6 ]

(* TODO so slow!!!! *)
(* let test2 = perfect 10000 = [ 8128; 496; 28; 6 ] *)

(* 問題14.15 *)
let one_to_n n = fold_right ( + ) (enumerate n) 0

let test1 = one_to_n 10 = 55

let test2 = one_to_n 100 = 5050

let rec fac n = if n = 0 then 1 else n * fac (n - 1)

let test1 = fac 2 = 2

let test2 = fac 3 = 6

let test3 = fac 4 = 24

let test4 = fac 5 = 120

let rec fac' n = fold_right ( * ) (enumerate n) 1

let test1 = fac' 2 = 2

let test2 = fac' 3 = 6

let test3 = fac' 4 = 24

let test4 = fac' 5 = 120

(* range 0 5 [0, 5), 0 <= n < 5 *)
let rec range s e = if s > e then [] else s :: range (s + 1) e

let test1 = range 0 5 = [ 0; 1; 2; 3; 4; 5 ]

(* 第15章 新しい形の再帰 *)
(* 一般の再帰 *)
(* Auxiliary *)

let rec aux_take' n lst p =
  (*predicate*)
  List.filter (fun a -> p a n) lst

(* 目的: lstの中からnより小さい要素を取り出す *)
let rec take_less n lst =
  match lst with
  | [] -> []
  | h :: t -> if h < n then h :: take_less n t else take_less n t

let rec take_less' n lst = List.filter (fun a -> a < n) lst

let rec take_less'' n lst = aux_take' n lst ( < )

(* Auxiliary *)
(* 目的: lstの中からnより大きい要素を取り出す *)
let rec take_greater n lst =
  match lst with
  | [] -> []
  | h :: t -> if h > n then h :: take_greater n t else take_greater n t

let rec take_greater' n lst = List.filter (fun a -> a < n) lst

let rec take_greater'' n lst = aux_take' n lst ( > )

let rec quick_sort l =
  (* 自明に答えがでるケースの条件 *)
  (* 自明に答えが出るーケース*)
  (*それ以外のケース*)
  match l with
  | [] -> []
  | h :: t -> quick_sort (take_less h t) @ [ h ] @ quick_sort (take_greater h t)

let rec quick_sort' l =
  (* 自明に答えがでるケースの条件 *)
  (* 自明に答えが出るーケース*)
  (*それ以外のケース*)
  match l with
  | [] -> []
  | h :: t ->
    quick_sort' (take_less' h t) @ [ h ] @ quick_sort' (take_greater' h t)

let rec quick_sort'' = function
  | [] -> []
  | h :: t ->
    quick_sort'' (take_less'' h t) @ [ h ] @ quick_sort'' (take_greater'' h t)

let test1 = quick_sort [] = []

let test2 = quick_sort [ 1 ] = [ 1 ]

let test3 = quick_sort [ 1; 2 ] = [ 1; 2 ]

let test4 = quick_sort [ 2; 1 ] = [ 1; 2 ]

let test5 = quick_sort [ 5; 4; 9; 8; 2; 3 ] = [ 2; 3; 4; 5; 8; 9 ]

let test1 = quick_sort'' [] = []

let test2 = quick_sort'' [ 1 ] = [ 1 ]

let test3 = quick_sort'' [ 1; 2 ] = [ 1; 2 ]

let test4 = quick_sort'' [ 2; 1 ] = [ 1; 2 ]

let test4 = quick_sort'' [ 2; 1 ]

let test5 = quick_sort'' [ 5; 4; 9; 8; 2; 3 ] = [ 2; 3; 4; 5; 8; 9 ]

let rec q_sort l =
  let take_less h lst = List.filter (fun a -> a < h) lst in
  let take_greater h lst = List.filter (fun a -> a > h) lst in
  let take_equal h lst = List.filter (fun a -> a = h) lst in
  match l with
  | [] -> []
  | h :: t ->
    q_sort (take_less h t) @ take_equal h l @ q_sort (take_greater h t)

let test1 = q_sort [] = []

let test2 = q_sort [ 1 ] = [ 1 ]

let test3 = q_sort [ 1; 2 ] = [ 1; 2 ]

let test4 = q_sort [ 2; 1 ] = [ 1; 2 ]

let test4 = q_sort [ 2; 1 ]

let test5 = q_sort [ 5; 4; 9; 8; 2; 3 ] = [ 2; 3; 4; 5; 8; 9 ]

let test4 = q_sort [ 1; 1 ] = [ 1; 1 ]

(* 15.4 停止性の判定 *)

(* n! *)
let rec fac n = if n <= 0 then 1 else n * fac (n - 1)

let rec dai_n_kou n = if n = 0 then 1.0 else dai_n_kou (n - 1) /. float_of_int n

(* eの近似値を求める *)
let rec e n =
  if dai_n_kou n < 0.00001 then dai_n_kou n else dai_n_kou n +. e (n + 1)

let rec e' n =
  let d = dai_n_kou n in
  if d < 0.00001 then d else d +. e (n + 1)

(* 15.2 ユークリッドの互除法 *)
(* Euclidean Algorithm *)

(* 2つの自然数a,bを取る a >= b *)
(* aとbの最大公約数rは、bとrの最大公約数と等しい*)
let rec gcd a b =
  let r = a mod b in
  let r' = b mod r in
  if r mod r' = 0 then r' else gcd a b

(* m >= n >= 0 *)
let rec gcd m n = if n = 0 then m else gcd n (m mod n)

let test1 = gcd 1071 1029 = 21

let range a b = if a > b then [] else a :: range (a + 1) b

let natural_2_n n = range 2 n

let test1 = natural_2_n 10 = [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

let rec eratosthenes = function
  | [] -> []
  | h :: t -> h :: eratosthenes (List.filter (fun n -> n mod h != 0) t)

let test1 = eratosthenes (natural_2_n 10)

let prime n = eratosthenes (natural_2_n n)

let test1 = prime 10 = [ 2; 3; 5; 7 ]

(* 15.6 最短距離最小の点の分離 *)
(* 最短距離が未確定の点集合V *)

(* 問題15.4 page.176 *)
(* eki_t型リストを受け取る。 (最短距離最小の駅,最短距離最小以外の駅のリスト)の組を返す。 *)
let rec saitan_wo_bunri eki_t_lst =
  match eki_t_lst with
  | [] -> []
  | h :: t -> []

(* 最短距離最小の駅を求めるため、10.2のminimumを参考にする *)
let rec minimum' = function
  | [] -> max_int
  | h :: t ->
    let t' = minimum' t in
    if h < t' then h else t'

let rec minimum_distance = function
  | [] -> { namae = ""; saitan_kyori = max_float; temae_list = [] }
  | h :: t ->
    let tmp_t = minimum_distance t in
    if h.saitan_kyori < tmp_t.saitan_kyori then h else tmp_t

(* page.128 ダイクストラ法の図解 *)
let test1 =
  minimum_distance [ { namae = "zero"; saitan_kyori = 0.; temae_list = [] } ]

let test2 =
  minimum_distance
    [ { namae = "a1"; saitan_kyori = 1.; temae_list = [] }
    ; { namae = "b2"; saitan_kyori = 2.; temae_list = [] }
    ; { namae = "c3"; saitan_kyori = 3.; temae_list = [] }
    ]

(* page.101 shukeiを参考に、組を返す。*)
let rec drop_eki eki lst = List.filter (fun e -> e <> eki) lst

let test1 =
  drop_eki
    { namae = "b2"; saitan_kyori = 2.; temae_list = [] }
    [ { namae = "a1"; saitan_kyori = 1.; temae_list = [] }
    ; { namae = "b2"; saitan_kyori = 2.; temae_list = [] }
    ; { namae = "c3"; saitan_kyori = 3.; temae_list = [] }
    ]

(* 最短距離最小の点の分離 eki_t list型を受け取って でsaitan_kyoriフィールドが最も小さいeki_t型とそれ以外のeki_t
   listを組にして返す。 *)
let rec saitan_wo_bunri eki_t_lst =
  let short_sta = minimum_distance eki_t_lst in
  (short_sta, drop_eki short_sta eki_t_lst)

let test1 =
  saitan_wo_bunri
    [ { namae = "a1"; saitan_kyori = 1.; temae_list = [] }
    ; { namae = "b2"; saitan_kyori = 2.; temae_list = [] }
    ; { namae = "c3"; saitan_kyori = 3.; temae_list = [] }
    ]

let test2 =
  saitan_wo_bunri
    [ { namae = "d11"; saitan_kyori = 11.; temae_list = [] }
    ; { namae = "e2"; saitan_kyori = 2.; temae_list = [] }
    ; { namae = "f33"; saitan_kyori = 33.; temae_list = [] }
    ]

type distance_t =
  { kyori : float (* 距離 *)
  ; total : float (* 距離の合計*)
  }

(* 先頭からリスト中の各店までの合計を計算する *)
(* accumulator is total distance. *)
let rec hojo lst total0 =
  match lst with
  | [] -> []
  | h :: t ->
    let total1 = h.kyori +. total0 in
    { kyori = h.kyori; total = total1 } :: hojo t total1

let rec total_distance lst = hojo lst 0.

(*todo: ｔｅｓｔ追加*)

(* 16.1 *)
let rec sum_list lst total =
  match lst with
  | [] -> []
  | h :: t -> (h + total) :: sum_list t (h + total)

let test1 = sum_list [ 3; 2; 1; 4 ] 0 = [ 3; 5; 6; 10 ]

let test2 = sum_list [ 1; 2; 3; 4 ] 0 = [ 1; 3; 6; 10 ]

(* 16.3 アキュムレーターの活用 *)
(* lstの逆順のリスト @ result を返す。*)
(* result accumulaterは、これまでの要素の逆順のリストを示す。 *)
let rec rev lst result =
  match lst with
  | [] -> lst @ result
  | h :: t -> rev t (h :: result)

(* 与えられたリストを逆順にして返す。*)
let rec reverse lst =
  let rec rev xs result =
    match xs with
    | [] -> xs @ result
    | h :: t -> rev t (h :: result)
  in
  rev lst []

let test1 = reverse [ 1; 2; 3; 4; 5 ] = [ 5; 4; 3; 2; 1 ]

(* 問題 16.2 *)
let rec fold_left f acc lst =
  match lst with
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

(* + (+ (+ (+ (+ 0 1) 2) 3) 4) 5 *)

let test1 = fold_left (fun a i -> a + i) 0 [ 1; 2; 3; 4; 5 ] = 15

(* 問題16.3 *)
(* ************************************************************************* *)
(* 最短距離最小の点の分離 eki_t list型を受け取って でsaitan_kyoriフィールドが最も小さい
 * ki_t型とそれ以外のeki_tlistを組にして返す。 *)
let rec saitan_wo_bunri eki_t_lst =
  let short_sta = minimum_distance eki_t_lst in
  (short_sta, drop_eki short_sta eki_t_lst)

let test1 =
  saitan_wo_bunri
    [ { namae = "a1"; saitan_kyori = 1.; temae_list = [] }
    ; { namae = "b2"; saitan_kyori = 2.; temae_list = [] }
    ; { namae = "c3"; saitan_kyori = 3.; temae_list = [] }
    ]

let rec koushin1' p q ekikan_lst =
  let kyori = get_ekikan_kyori p.namae q.namae ekikan_lst in
  if kyori = infinity then q
  else if p.saitan_kyori +. kyori < q.saitan_kyori then
    { q with
      saitan_kyori = p.saitan_kyori +. kyori
    ; temae_list = q.namae :: p.temae_list
    }
  else q

let p1 = { namae = "代々木上原"; saitan_kyori = 3.; temae_list = [] }

let q1 = { namae = "代々木公園"; saitan_kyori = 10.; temae_list = [] }

(* {kiten="代々木上原"; shuten="代々木公園"; keiyu="千代田線"; kyori=1.0; jikan=2}; *)

let test1 =
  let p = { namae = "代々木上原"; saitan_kyori = 3.; temae_list = [] } in
  let q = { namae = "代々木公園"; saitan_kyori = 10.; temae_list = [] } in
  let actual =
    { namae = "代々木公園"; saitan_kyori = 4.; temae_list = [ "代々木公園" ] }
  in
  koushin1' p q global_ekikan_list = actual

(* 13.7 *)
(* eki_t型pとeki_t list型の各要素eを比較し、p経由でeにいった場合の最短距離が
 * すでにpに設定されている最短距離より小さい場合は、eの最短距離saitan_kyoriと、 
 * 経由リストtemae_listを更新する。*)
let rec koushin' p v ekikan_lst = map (fun e -> koushin1' p e ekikan_lst) v
(* todo: test *)

(* 16.4 *)
(* 目的: eki_t list型の未確定の駅リストv とekikan_t list型の駅間リストを受け取ったら
 * dijkstraのアルゴリズムにしたがって
 * 各駅について最短距離と最短経路が正しく入ったeki_t listを返すdijkstra_mainを作れ *)
(* 最短距離最小の点を分離 saitan_wo_bunri *)
(* 更新 koushin *)

let rec dijkstra_main pending_ls ekikan_ls =
  match pending_ls with
  | [] -> []
  | h :: t ->
    let saitan, other_ls = saitan_wo_bunri pending_ls in
    let updated_eki_lst = koushin' saitan other_ls ekikan_ls in
    saitan :: dijkstra_main updated_eki_lst ekikan_ls

(* todo テスト *)

(* 問題16.5 *)
let rec find_shuten shuten_kanji = function
  | [] -> { namae = ""; saitan_kyori = infinity; temae_list = [] }
  | h :: t -> if h.namae = shuten_kanji then h else find_shuten shuten_kanji t

let rec dijkstra shiten shuten =
  let ekimei_list = seiretsu global_ekimei_list in
  let shiten_kanji = romaji_to_kanji shiten ekimei_list in
  let shuten_kanji = romaji_to_kanji shuten ekimei_list in
  let ekimei_list2 = make_initial_eki_list ekimei_list shiten_kanji in
  let ekimei_list3 = dijkstra_main ekimei_list2 global_ekikan_list in
  find_shuten shuten_kanji ekimei_list3

(* todo:オリジナルテストにする *)

let test1 =
  dijkstra "shibuya" "gokokuji"
  = { namae = "護国寺"
    ; saitan_kyori = 9.8
    ; temae_list =
        [ "護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; "青山一丁目"; "表参道"; "渋谷" ]
    }

let test1 = dijkstra "shibuya" "gokokuji"

let test2 =
  dijkstra "myogadani" "meguro"
  = { namae = "目黒"
    ; saitan_kyori = 12.7000000000000028
    ; temae_list =
        [ "目黒"
        ; "白金台"
        ; "白金高輪"
        ; "麻布十番"
        ; "六本木一丁目"
        ; "溜池山王"
        ; "永田町"
        ; "麹町"
        ; "市ヶ谷"
        ; "飯田橋"
        ; "後楽園"
        ; "茗荷谷"
        ]
    }

