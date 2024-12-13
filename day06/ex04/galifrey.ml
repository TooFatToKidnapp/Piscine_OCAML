class galifrey (daleks) (doctors) (people) =
  object
    val dalek_army: Dalek.dalek Army.army = daleks
    val doctor_army: Doctor.doctor Army.army  = doctors
    val people_army: People.people Army.army  = people
    method do_time_war = match (dalek_army#check_alive, doctor_army#check_alive, people_army#check_alive) with
      | (false, false, false) -> print_endline "End of the war. Everyone is DEAD!"
      | (true, false, false) -> print_endline "Only dalek left alive." ; (dalek_army#get_fighter 0)#talk
      | (false, true, false) -> print_endline "Only doctor left alive. " ; let d = doctor_army#get_fighter 0 in d#talk ; d#travel_in_time 2020 2100
      | (false, false, true) -> print_endline "Only people left alive."
      | (true, false, true) -> print_endline "Daleks and People are alive. Fight is on." ; (dalek_army#get_fighter 0)#exterminate (people_army#get_fighter 0) ; people_army#delete ; let war = new galifrey dalek_army doctor_army people_army in war#do_time_war
      | (false, true, true) -> print_endline "People and Doctor are alive. Peace." ; (people_army#get_fighter 0)#talk ; (doctor_army#get_fighter 0)#talk
      | (true, true, false) ->
        begin
          match Random.int 4 with
          | 0 -> (doctor_army#get_fighter 0)#use_sonic_screwdriver ; (dalek_army#get_fighter 0)#die ; dalek_army#delete ; let war = new galifrey dalek_army doctor_army people_army in war#do_time_war
          | 1 -> (doctor_army#get_fighter 0)#travel_in_time 10 10 ; (dalek_army#get_fighter 0)#die ; dalek_army#delete ; let war = new galifrey dalek_army doctor_army people_army in war#do_time_war
          | 2 -> (dalek_army#get_fighter 0)#talk ; let war = new galifrey dalek_army doctor_army people_army in war#do_time_war
          | 3 -> (doctor_army#get_fighter 0)#talk ; let war = new galifrey dalek_army doctor_army people_army in war#do_time_war
          | _ -> print_endline "WAR IS OVER."
        end
      | (true, true, true) ->
        begin
            match Random.int 5 with
            | 0 -> (people_army#get_fighter 0)#talk ; (doctor_army#get_fighter 0)#talk ; (dalek_army#get_fighter 0)#talk ; (dalek_army#get_fighter 0)#exterminate (people_army#get_fighter 0) ; people_army#delete ; let war = new galifrey dalek_army doctor_army people_army in war#do_time_war
            | 1 -> (doctor_army#get_fighter 0)#use_sonic_screwdriver ;  (dalek_army#get_fighter 0)#die ; dalek_army#delete ; let war = new galifrey dalek_army doctor_army people_army in war#do_time_war
            | 2 -> (dalek_army#get_fighter 0)#talk ; let war = new galifrey dalek_army doctor_army people_army in war#do_time_war
            | 3 -> (doctor_army#get_fighter 0)#travel_in_time 10 10; (dalek_army#get_fighter 0)#die ; dalek_army#delete ;let war = new galifrey dalek_army doctor_army people_army in war#do_time_war
            | 4 -> (doctor_army#get_fighter 0)#talk ; let war = new galifrey dalek_army doctor_army people_army in war#do_time_war
            | _ -> print_endline "WAR IS OVER."
        end
  end
