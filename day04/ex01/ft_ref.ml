type 'a ft_ref = { mutable contents: 'a }

let return (contents: 'a) : 'a ft_ref =
  { contents }


let get (r: 'a ft_ref): 'a =
  r.contents


let set (r: 'a ft_ref) (new_content: 'a) : unit =
  r.contents <- new_content

let bind (r: 'a ft_ref) (fn: 'a -> 'b ft_ref) : 'b ft_ref =
  fn r.contents


let () =
  let t = (return 1) in
  let new_t = bind t (fun x -> return (x + 1)) in
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

