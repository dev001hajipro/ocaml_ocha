(* Want to interrupt while run #use. *)

print_string "hello";;

let s = Sys.sigint;; (* Interractive interrupt ctlr-C *)

print_string "word";;