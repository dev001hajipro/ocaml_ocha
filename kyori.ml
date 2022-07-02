(* 目的: 原点から受け取った座標pairまでの距離を求める *)
(* kyori : float * float -> float *)
(* 今回組pairを使った。
 * 入力が組の場合、テンプレートとしてmatch式の部分は書ける。
 * = match pair with
 *   (a, b) -> 0
*)
let kyori pair = match pair with
  (a, b) -> sqrt (a *. a +. b *. b)


(* テスト *)
let test1 = kyori (3.0, 4.0) = 5.0
let test2 = kyori (5.0, 12.0) = 13.0
let test3 = kyori (8.0, 15.0) = 17.0
