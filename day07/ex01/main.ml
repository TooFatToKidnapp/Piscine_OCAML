(* https://en.wikipedia.org/wiki/List_of_inorganic_compounds#V_2 *)

let () =
  let m = new Water.water in
  print_endline m#to_string;
  let cd = new Carbon_dioxide.carbon_dioxide in
  print_endline cd#to_string;
  let sc = new Sodium_carbonate.sodium_carbonate in
  print_endline sc#to_string;
  let zo = new Zinc_oxide.zinc_oxide in
  print_endline zo#to_string;
  let sh = new Sodium_helide.sodium_helide in
  print_endline sh#to_string;

  print_string "water == sodium_helide : ";
  print_endline (string_of_bool (m#equals sh));

  print_string "water == water : ";
  print_endline (string_of_bool (m#equals (new Water.water)));
