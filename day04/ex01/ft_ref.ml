type 'a ft_ref = { mutable contents: 'a }

let return (content: 'a) : 'a ft_ref =
  { contents = content }


let get (r: 'a ft_ref): 'a =
  r.contents


let set (r: 'a ft_ref) (content: 'a) : unit =
  r.contents <- content

let bind (r: 'a ft_ref) (fn: 'a -> 'b ft_ref) : 'b ft_ref =
  fn r.contents


let () =
  let t = (return 1)
  in let new_t = bind t (fun x -> return (x + 1))
  in
  print_string "t = ";
  print_int (get t);
  print_char '\n';
  print_string "new_t = ";
  print_int (get new_t);
  print_char '\n';
  set t 3;
  print_string "t = ";
  print_int (get t);
  print_char '\n';

