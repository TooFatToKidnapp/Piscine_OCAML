let ft_sum (fn: int -> float) l u : float =
  if u < l then nan
  else
    let rec aux acc i =
      if i > u then acc
      else aux (acc +. fn i) (i + 1)
    in
    aux 0.0 l


let () =
  print_endline (string_of_float (ft_sum (fun i -> float_of_int (i * i)) 1 10));
  print_endline (string_of_float (ft_sum (fun i -> float_of_int (i * i)) 10 9));
  print_endline (string_of_float (ft_sum (fun i -> float_of_int (i * i)) 10 10));
