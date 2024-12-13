class ['a] army (member_list :'a list)=
  object
    val mutable members = member_list
    method add member =
      members <- (member :: members)
    method delete =
      match members with
      | [] -> ()
      | _ :: t -> members <- t

    method get_army_info =
      print_endline "Current Army Troops:";
      let rec aux = function
      | [] -> ()
      | h :: t -> print_endline (h#to_string); aux t;
    in
    aux members

    method check_alive = match members with | [] -> false | _ -> true
    method get_fighter (x:int) = List.nth members x
  end
