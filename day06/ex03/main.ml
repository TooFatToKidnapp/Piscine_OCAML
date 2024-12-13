let () =
  print_endline "Dalek Army";
  let dalek1 = new Dalek.dalek in
  let dalek2 = new Dalek.dalek in
  let dalek3 = new Dalek.dalek in
  let a = new Army.army [dalek1; dalek2] in
  a#add dalek3;
  a#get_army_info;
  a#delete;
  a#get_army_info;

  print_endline "Doctor Army";
  let doctor1 = new Doctor.doctor in
  let doctor2 = new Doctor.doctor in
  let doctor3 = new Doctor.doctor in
  let a = new Army.army [doctor1; doctor2] in
  a#add doctor3;
  a#get_army_info;
  a#delete;
  a#get_army_info;

  print_endline "People Army";
  let person1 = new People.people "Alice" in
  let person2 = new People.people "Bob" in
  let person3 = new People.people "Charlie" in
  let a = new Army.army [person1; person2] in
  a#add person3;
  a#get_army_info;
  a#delete;
  a#get_army_info;

