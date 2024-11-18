let ft_print_comb() =
  let rec print_next_permutation (first:int) (second: int) (third: int) =
    if first <= 7 then (
      if second <= 8 then (
        if third <= 9 then (
          print_int first;
          print_int second;
          print_int third;
          if first != 7 || second != 8 || third != 9 then
            print_string ", ";
            print_next_permutation first second (third + 1)
        ) else
            print_next_permutation first (second + 1) (second +  2)
      ) else
        print_next_permutation (first + 1) (first + 2) (first + 3)
    )
  in
  print_next_permutation 0 1 2;
  print_string "\n"

let () = ft_print_comb()



