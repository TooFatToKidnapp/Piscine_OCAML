
let () =
    let people = new People.people "Xoriman" in
    let doctor = new Doctor.doctor in
    let newDalek = new Dalek.dalek in
    print_endline newDalek#to_string ;
    newDalek#talk ; print_char '\n';
    newDalek#exterminate people ; print_char '\n';
    newDalek#exterminate doctor#get_sidekick ; print_char '\n';
    print_endline newDalek#to_string ;
    newDalek#talk ; print_char '\n';
    doctor#use_sonic_screwdriver ; print_char '\n' ;
    print_endline doctor#to_string;
    newDalek#die
