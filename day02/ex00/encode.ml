let encode (lst : 'a list): (int * 'a) list =
  let rec total l last_elem count =
    match l with
    | [] -> [(count, last_elem)]
    | h :: t -> if last_elem = h then total t h (count + 1) else (count, last_elem) :: (total t h 1)
  in
  match lst with
  | [] -> []
  | h::t -> total t h 1


  let rec print_list = function
  | [] -> print_char '\n'
  | (a, b) :: l ->
    print_int a;
    print_char b;
    print_list l

let () =
  print_list (encode ['a'; 'a'; 'a'; 'b'; 'b']) ;
  print_list (encode ['b']) ;
  print_list (encode [])


