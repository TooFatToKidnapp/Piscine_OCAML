class dalek =
  let generate_name () =
    Random.self_init ();
    let rec aux acc count =
      match count with
      | 0 -> acc
      | _ -> let c_as_int = Random.int 26 in
              if (Random.int 100) mod 2 = 0 then aux (acc ^ String.make 1 (char_of_int (c_as_int + 65)) ) (count - 1)
              else aux (acc ^  String.make 1 (char_of_int (c_as_int + 97))) (count - 1)
    in
    "Dalek" ^ (aux "" 3)
  in
  object
    val name = generate_name ()
    val mutable hp = 100
    val mutable shield = true
    method to_string =
      "Dalek -> name: " ^ name ^ " | hp: " ^ (string_of_int hp) ^ " | shield: " ^ (string_of_bool shield)
    method talk =
      Random.self_init();
      match Random.int 3 with
      | 0 -> print_endline "Explain! Explain!"
      | 1 -> print_endline "Exterminate! Exterminate!"
      | 2 -> print_endline "I obey!"
      | _ -> print_endline "You are the Doctor! You are the enemy of the Daleks!"

    method exterminate (persone_to_kill: People.people) =
      print_endline (persone_to_kill#get_name ^ " got exterminated by " ^ name);
      persone_to_kill#die;
      match shield with
      | true -> shield <- false
      | false ->  shield <- true;

    method die =
      hp <- 0;
      print_endline "Emergency Temporal Shift!"


end
