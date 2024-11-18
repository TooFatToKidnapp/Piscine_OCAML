(* https://www.youtube.com/watch?v=OLqdJMjzib8 *)
let rec converges fn x (n: int) : bool =
  if n <= 0 then false
  else if fn x = x then true
  else converges fn (fn x) (n - 1)


let () =
  print_endline (string_of_bool (converges (fun x -> x *. 2.0) 2. 5));
  print_endline (string_of_bool (converges (( * ) 2) 2 5));
  print_endline (string_of_bool (converges (fun x -> x / 2) 2 3));
  print_endline (string_of_bool (converges (fun x -> x / 2) 2 2));


