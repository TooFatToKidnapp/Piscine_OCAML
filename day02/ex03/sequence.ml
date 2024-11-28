let sequence n = begin
  if n < 0 then ""
  else begin
    let rec aux c start = begin
      let rec build_sequence acc elm count = function
        | [] -> acc @ [elm ^ (string_of_int count)]
        | h::t -> if h = elm then build_sequence acc h (count + 1) t
                  else build_sequence (acc @ [elm ^ (string_of_int count)]) h 1 t
      in
      if c = 0 then start
      else match start with
      | [] -> []
      | h::t -> aux (c - 1) (build_sequence [] h 0 t)
    end
    in
      let rec list_as_str acc = function
      | [] -> acc
      | h::t -> list_as_str (acc ^ h) t
      in
      list_as_str "" (aux n ["1"])
  end
end

(*
  2
  aux 2 ["1"]
    build : [] "1" 0 [] ->

*)

let () =
  print_endline ("[" ^ (sequence 0) ^ "]");
  print_endline ("[" ^ (sequence 1) ^ "]");
  print_endline ("[" ^ (sequence 2) ^ "]");
  print_endline ("[" ^ (sequence 3) ^ "]");
  print_endline ("[" ^ (sequence 4) ^ "]");
