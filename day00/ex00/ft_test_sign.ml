let ft_test_sign (x: int) : unit =
  if x >= 0 then
    print_endline "positive"
  else
    print_endline "negative"


let () =
    ft_test_sign 1;
    ft_test_sign 0;
    ft_test_sign (-1);
