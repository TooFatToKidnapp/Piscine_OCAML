class people (initial_name: string) =
  object
    val mutable name = initial_name
    val mutable hp = 100

    initializer
      print_endline ("Creating a new person named " ^ name)

    method to_string =
      "people -> name: " ^ name ^  " | hp: " ^ string_of_int hp

    method talk =
      print_endline ("I'm " ^ name ^ " do you know the Doctor?")

    method die =
      hp <- 0;
      print_endline "Aaaarghh!"

    method get_name =
      name
end
