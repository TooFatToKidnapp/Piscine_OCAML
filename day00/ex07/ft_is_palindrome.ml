let ft_is_palindrome (s: string): bool =
  let rec check_chars (start: int) (end_c: int) : bool =
    if start == 0 && end_c == -1 then
      true
    else (
      if String.get s start == String.get s end_c then
        if start < end_c then
          check_chars (start + 1) (end_c - 1)
        else
          true
        else
          false
    )
    in
      check_chars 0 ((String.length s) - 1)


let () =
  if ft_is_palindrome "radar" then
    print_endline "True"
  else
    print_endline "False";
  if ft_is_palindrome "madam" then
      print_endline "True"
  else
    print_endline "False";
  if ft_is_palindrome "car" then
    print_endline "True"
  else
    print_endline "False";
  if ft_is_palindrome "" then
    print_endline "True"
  else
    print_endline "False"
