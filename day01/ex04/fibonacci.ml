(* https://abitofocaml.weebly.com/131-tail-recursion.html *)
(* https://www.youtube.com/watch?v=_JtPhF8MshA *)
let fibonacci n : int =
  if n < 0 then -1
  else
    let rec aux n a b =
      if n = 0 then a
      else if n = 1 then b
      else aux (n - 1) b (a + b)
    in
    aux n 0 1


let () =
  print_endline (string_of_int (fibonacci (-42)));
  print_endline (string_of_int (fibonacci 1));
  print_endline (string_of_int (fibonacci 3));
  print_endline (string_of_int (fibonacci 6));
