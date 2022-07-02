let read filename =
  let f = open_in filename in
  try
    while true do
      print_endline (input_line f)
    done
  with
  | End_of_file -> ()

let write_hello filename =
  let oc = open_out filename in
  output_string oc "write from OCaml😍\n";
  output_string oc "write from OCaml😍\n";
  output_string oc "write from OCaml😍\n";
  output_string oc "write from OCaml😍\n";
  close_out oc

let delete_file filename =
  Sys.remove filename

let () = 
  let f = "zhello.txt" in
  write_hello f;
  read f;
  delete_file f

