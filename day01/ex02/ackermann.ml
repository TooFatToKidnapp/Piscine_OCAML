(* https://www.youtube.com/watch?v=i7sm9dzFtEI *)
let rec ackermann m n : int =
   if m = 0 then n + 1
   else if m > 0 && n == 0 then ackermann (m-1) 1
   else if m > 0 && n > 0 then ackermann (m-1) (ackermann m (n-1))
   else -1


let () =
      print_int (ackermann (-1) 7);
      print_endline "";
      print_int (ackermann 0 0);
      print_endline "";
      print_int (ackermann 2 3);
      print_endline "";
      print_int (ackermann 4 1);
      print_endline "";
