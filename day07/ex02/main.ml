let () =
  let m = new Methane.methane in
  let e = new Ethane.ethane in
  let o = new Octane.octane in
  print_endline (m#to_string);
  print_endline (e#to_string);
  print_endline (o#to_string);
  let m2 = new Methane.methane in
  print_endline (string_of_bool (m#equals m2)) ;
  print_endline (string_of_bool (m#equals o))
