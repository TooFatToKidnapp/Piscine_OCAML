let ft_print_alphabet () =
  let rec get_char(c: int) : unit =
    if c < 122 then (
      let to_print = char_of_int c in
      print_char to_print;
      get_char (c + 1)
    ) else
      ()
    in
  get_char 97;
  print_char '\n'



let () = ft_print_alphabet ()
