let rec hilo n =
  let () = print_string "type anumber:" in
  let i = read_int () in
  if i = n
  then (
    print_string "やったー！";
    print_newline ();
    print_newline ())
  else
    let () =
      if i < n
      then (
        print_string "Higher";
        print_newline ())
      else
        let () = print_string "Lower" in
        print_newline ()
    in
    hilo n
