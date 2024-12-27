let print_int_set s =
  Set.Set.foreach s (fun x -> Printf.printf "%d " x) ;
  Printf.printf "\n"

let () =
  let s1 = Set.Set.union (Set.Set.union (Set.Set.return (-1)) (Set.Set.return 0)) (Set.Set.union (Set.Set.return 5) (Set.Set.return 10)) in
  let s2 = Set.Set.union (Set.Set.union (Set.Set.return 3) (Set.Set.return (-100))) (Set.Set.union (Set.Set.return 0) (Set.Set.return 3356)) in
  let s3 = Set.Set.union (Set.Set.union (Set.Set.return 1337) (Set.Set.return 9988)) (Set.Set.union (Set.Set.return 65) (Set.Set.return 77)) in
  Printf.printf "s1 = ";
  print_int_set s1;
  Printf.printf "s2 = ";
  print_int_set s2;
  Printf.printf "s3 = ";
  print_int_set s3;
  Printf.printf "s1 + s2 = ";
  print_int_set (Set.Set.union s1 s2);
  Printf.printf "diff s1 s2 = ";
  print_int_set (Set.Set.diff s1 s2);
  Printf.printf "filter s1 = ";
  print_int_set (Set.Set.filter s1 (fun x -> x > 0));
  Printf.printf "inter s1 s2 = ";
  print_int_set (Set.Set.inter s1 s2);
  Printf.printf "foreach s1 to string ";
  Set.Set.foreach s1 (fun x -> print_string ((string_of_int x) ^ " "));
  print_char '\n';
  Printf.printf "for_all s3 are > 0 = %b\n" (Set.Set.for_all s3 (fun x -> x > 0));
  Printf.printf "1337 existes in s3 : %b\n" (Set.Set.existes s3 (fun x -> x = 1337));
