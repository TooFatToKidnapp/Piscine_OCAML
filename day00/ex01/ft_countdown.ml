let rec ft_countdown (n: int) : unit =
  if n >= 0 then (
    print_int n;
    print_char '\n';
    ft_countdown (n - 1);
  )
  else
    ()

let () =
    ft_countdown 3;
    ft_countdown 0;
    ft_countdown (-4);
