let rec echo () =
  print_string (read_line ());
  print_newline ();
  echo ()

(* entry point *)
let () =
  print_string "じゃんけんゲーム";
  print_newline ();
  print_string "エコーサンプル";
  print_newline ();
  echo ()

