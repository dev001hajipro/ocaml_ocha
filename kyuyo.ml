(*
  給与は働いた時間掛ける950円に基本給100円
 *)
(* 優遇時給 *)
let yugu_jikyu = 980
let jikyu = 950
let kihonkyu = 100

(* val kyuyo : int -> int = <fun> *)
(* let kyuyo t = jikyu * t + kihonkyu *)
let kyuyo t =
  kihonkyu + t * (if t < 30 then  jikyu else yugu_jikyu)

let test1 = kyuyo 25 = 23850
let test2 = kyuyo 28 = 26700
let test3 = kyuyo 31 = 30480