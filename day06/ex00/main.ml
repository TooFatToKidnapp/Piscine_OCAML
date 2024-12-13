let () =
  let person = new People.people "Ali" in
  print_endline person#to_string;
  person#talk;
  person#die


