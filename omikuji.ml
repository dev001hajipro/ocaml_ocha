
let omikuji () = match Random.int 10 with
 0 -> "大吉😆"
| 1 -> "中吉"
| 3 -> "小吉"
| 9 -> "凶😖"
| _ -> "吉"


let test1 = omikuji ()