let repeat_string ?(str="x") (n: int) : string =
  if n < 0 then "Error"
  else
    let rec rep acc n =
      if n <= 0 then acc
      else rep (acc ^ str) (n - 1)
    in
  rep "" n


let () =
  print_endline (repeat_string (-1));
  print_endline (repeat_string 0);
  print_endline (repeat_string 1 ~str:"Toto");
  print_endline (repeat_string 2);
  print_endline (repeat_string ~str:"a" 5);
  print_endline (repeat_string ~str:"what" 3);

