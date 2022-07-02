(* degisn recipe *)
(* 1.目的を書く *)
(* 所持金nが与えられたとき、
 * 126円のチョコレートがいくつ買えるかを返す
 * chocolate関数 *)
let chocolate n = n / 126 

(* 2.例を書く *)
(* 2.2 例をテストにする *)
let test0 = (chocolate 10 = 0)
let test1 = (chocolate 200 = 1)
let test2 = (chocolate 300 = 2)
let test3 = (chocolate 400 = 3)
let test5 = (chocolate 500 = 3)
let test6 = (chocolate 600 = 4)
