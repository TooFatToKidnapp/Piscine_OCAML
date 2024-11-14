let ft_rot_n (shift: int) (str: string): string =
  let fn (c: char) : char =
    if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') then
      let new_char_as_int = (int_of_char c + shift) in
      if (c <= 'Z' && new_char_as_int > 90) || (c <= 'z' && new_char_as_int > 122) then
        char_of_int (new_char_as_int - 26)
      else
        char_of_int new_char_as_int
    else
      c
    in
    String.map fn str



let () =
  print_endline ("[" ^ (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz") ^ "]");
  print_endline ("[" ^ (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz") ^ "]");
  print_endline ("[" ^ (ft_rot_n 2 "OI2EAS67B9") ^ "]");
  print_endline ("[" ^ (ft_rot_n 0 "Damned !") ^ "]");
  print_endline ("[" ^ (ft_rot_n 42 "") ^ "]");
  print_endline ("[" ^ (ft_rot_n 1 "NBzlk qnbjr !") ^ "]");

