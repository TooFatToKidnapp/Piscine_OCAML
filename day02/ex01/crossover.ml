let crossover lst lst2 =
  if lst = [] || lst2 = [] then []
  else if lst = lst2 then lst
  else
    let rec is_elem_exist elm = function
    | [] -> false
    | h::t -> if h = elm then true else is_elem_exist elm t
  in
    let rec aux l1 l2  =
    match (l1, l2) with
    | ([], _) | (_, []) -> []
    | (h1::t1 , h2::t2) ->
      if is_elem_exist h1 l2 then h1 :: aux t1 l2
      else aux t1 l2
    in
    aux lst lst2


let rec print_list = function
| [] -> print_char '\n'
| a:: l ->
  print_char a;
  print_list l


let () =
  print_list (crossover ['a'; 'b'; 'a'] ['a'; 'a'; 'z'; 'b']);
  print_list (crossover ['a'; 'b'; 'a'] ['a'; 'b'; 'a']);
  print_list (crossover [] ['d'; 'a'; 'z'; 'b']);
