class doctor =
  object
    val name = "Delta Cubed Sigma X Squared"
    val mutable age = 900
    val sidekick = new People.people "Martha Jones"
    val mutable hp = 100

    method to_string =
      "Doctor -> name: " ^ name ^ " | age: " ^ (string_of_int age) ^ " | hp: " ^ (string_of_int hp)
      ^ "| sidekick: " ^ sidekick#to_string
    method talk = print_endline "Hi! I\'m the Doctor!"
    initializer
      print_endline "Did someone call a Doctor?"

    method travel_in_time start arrival =
        print_endline "               ___";
        print_endline "       _______(_@_)_______";
        print_endline "       | POLICE      BOX |";
        print_endline "       |_________________|";
        print_endline "        | _____ | _____ |";
        print_endline "        | |###| | |###| |";
        print_endline "        | |###| | |###| |";
        print_endline "        | _____ | _____ |";
        print_endline "        | || || | || || |";
        print_endline "        | ||_|| | ||_|| |";
        print_endline "        | _____ |$_____ |";
        print_endline "        | || || | || || |";
        print_endline "        | ||_|| | ||_|| |";
        print_endline "        | _____ | _____ |";
        print_endline "        | || || | || || |";
        print_endline "        | ||_|| | ||_|| |";
        print_endline "        |       |       |";
        print_endline "        *****************";
       match (arrival - start) with
       | x when x < 0 && age + x > 0 -> print_endline ("The Doctor travelled " ^ string_of_int (-1 * x) ^ " years in the past"); age <- age + x;
       | x when x < 0 && age + x < 0 -> print_endline "The Doctor cant travel to that year!"
       | x -> print_endline ("The Doctor travelled " ^ string_of_int (x) ^ " years in the future"); age <- age + x

    method use_sonic_screwdriver =
      print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

    method get_sidekick =
      sidekick

    method private regenerate = hp <- 100
end
