
let omikuji () = match Random.int 10 with
 0 -> "ε€§επ"
| 1 -> "δΈ­ε"
| 3 -> "ε°ε"
| 9 -> "εΆπ"
| _ -> "ε"


let test1 = omikuji ()