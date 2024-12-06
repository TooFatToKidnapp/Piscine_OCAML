let sequence n =
  let rec get_seq_len l e i =
    match l with
    | h::t when h = e -> get_seq_len t e (i + 1)
    | _ -> i
  in
  let rec skip_seq l e =
    match l with
    | h::t when h = e -> skip_seq t e
    | _ -> l
  in
  let rec get_next_seq l =
    match l with
    | [] -> []
    | h::t ->
      let len = get_seq_len t h 1 in
      len :: h :: get_next_seq (skip_seq t h)
  in
  let rec generate_seq l n =
    match n with
    | _ when n <= 0 -> l
    | _ -> generate_seq (get_next_seq l) (n - 1)
  in
  let rec seq_from_str str l =
    match l with
    | [] -> str
    | h::t -> seq_from_str (str ^ (string_of_int h)) t
  in
    seq_from_str "" (generate_seq [1] n)


let () =
  print_endline (sequence 0);
  print_endline (sequence 1);
  print_endline (sequence 2);
  print_endline (sequence 3);
  print_endline (sequence 4);
  print_endline (sequence 5);
  print_endline (sequence 6);
