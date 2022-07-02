(* データ定義 *)
(* 学生一人分のデータを表す型 *)
type gakusei_t = {
    namae: string;      (* 名前 *)
    tensuu: int;        (* 点数 *)
    seiseki: string;    (* 成績 *)
}

(* 学生データの例 *)
let gakusei1 = {namae="asai"; tensuu=98; seiseki="A"}
let gakusei2 = {namae="okita"; tensuu=38; seiseki="D"}
let gakusei3 = {namae="tanaka"; tensuu=81; seiseki="B"}

(* 目的: 学生の成績データを受け取ったら
 * 成績通知文を返す。
 *)
(* tsuuchi : gakusei_t -> string *)
let tsuuchi gakusei = match gakusei with
  {namae=n; tensuu=t; seiseki=s} -> 
  n ^ " さんの点数は " ^ string_of_int t ^ " 点で成績は " ^ s ^ " です。"


(* テスト *)
let test1 = tsuuchi gakusei1 = "asai さんの点数は 98 点で成績は A です。"
let test2 = tsuuchi gakusei2 = "okita さんの点数は 38 点で成績は D です。"
let test3 = tsuuchi gakusei3 = "tanaka さんの点数は 81 点で成績は B です。"
