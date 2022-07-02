(* ** Design Recipt ** *)
(*1.目的：所持金が与えられたら、126円のチョコレートをいくつ買えるかを求める*)
(* 2.型を考える *)
(* chocolate : int -> int *)
(* ファイルの場合;;必要なし *)
let chocolate n = n / 126


(* 3.例を考える=>テストプログラムとする *)
let test1 = (chocolate 100 = 0)
let test2 = (chocolate 252 = 2)
let test3 = (chocolate 500 = 3)
