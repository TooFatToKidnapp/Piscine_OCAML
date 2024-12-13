let () =
  let doc = new Doctor.doctor in

  print_endline (doc#to_string);

  doc#talk;

  doc#travel_in_time 2020 1000;
  print_endline (doc#to_string);
  
  doc#travel_in_time 1980 2020;
  print_endline (doc#to_string);

  doc#use_sonic_screwdriver;


