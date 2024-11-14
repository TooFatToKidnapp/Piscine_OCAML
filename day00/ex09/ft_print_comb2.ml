let ft_print_comb2 () =
  let rec print_next_permutation (a, b) =
    if a <= 98 then (
      if b <= 99 then (
        print_int a;
        print_char ' ';
        print_int b;
        if a != 98 || b != 99 then
          print_char ',';
          print_char ' ';
        if b < 99 then
          print_next_permutation (a, b + 1)
        else
          print_next_permutation (a + 1, a + 2)
      )
    )
  in
  print_next_permutation (0, 1);
  print_char '\n'


let () =
  ft_print_comb2()
