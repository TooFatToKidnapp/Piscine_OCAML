class people (initial_name: string) =
  object
    val name = initial_name
    val hp = 100

    initializer begin
      print_string "Creating a new person named ";
      print_endline name
    end
    method to_string =
      "people -> name: " ^ name ^  " | hp: " ^ string_of_int hp

    method talk =
      print_endline ("I'm " ^ name ^ " do you know the Doctor?")

    method die =
      print_endline "Aaaarghh!"

end
