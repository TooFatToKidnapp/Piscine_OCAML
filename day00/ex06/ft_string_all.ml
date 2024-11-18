let ft_string_all (fn: char -> bool) (str: string): bool =
  let rec apply_fn (index: int): bool =
    if index >= String.length str then
      true
    else (
      let c = String.get str index in
      if fn c then
        apply_fn (index + 1)
      else
          false
    ) in
    apply_fn 0


let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = (c >= '0' && c <= '9')

let () =
      if ft_string_all is_alpha "hello world!" then
        print_endline "returned true"
      else
        print_endline "returned false";

      if ft_string_all is_digit "123456789" then
        print_endline "returned true"
      else
        print_endline "returned false";

      if ft_string_all is_digit "12EAS67B9" then
        print_endline "returned true"
      else
        print_endline "returned false";
