let iter (f: int -> int) (x: int) (n: int) : int =
  if n < 0 then -1
  else
    let rec aux count acc : int =
      if count = 0 then acc
      else aux (count - 1) (f acc)
    in
    aux n x




let () =
  print_endline (string_of_int (iter (fun x -> x * x) 2 4));
  print_endline (string_of_int (iter (fun x -> x * 2) 2 4));
  print_endline (string_of_int (iter (fun x -> x * x) 2 (-1)));
