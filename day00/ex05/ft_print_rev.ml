let ft_print_rev (str: string) : unit =
  let rec rev(index: int): unit =
    if index >= 0 then (
      print_char (String.get str index);
      rev (index - 1);
    ) else
      ()
    in
    rev ((String.length str) - 1);
    print_char '\n'


let () =
    ft_print_rev "Hello World!";
    ft_print_rev ""
