let () =
  let z = new Zinc.zinc in
  print_endline (z#to_string);
  let c = new Carbon.carbon in
  print_endline (c#to_string);
  let b = new Boron.boron in
  print_endline (b#to_string);
  let he = new Helium.helium in
  print_endline (he#to_string);
  let hy = new Hydrogen.hydrogen in
  print_endline (hy#to_string);
  let o = new Oxygen.oxygen in
  print_endline (o#to_string);

  print_string "zinc == carbon : ";
  print_endline (string_of_bool (z#equals c));

  print_string "zinc == zinc : ";
  print_endline (string_of_bool (z#equals (new Zinc.zinc)));
