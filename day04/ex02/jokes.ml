
let () =
  let arr_of_jokes = [|
    "What does a baby computer call its father? Data.";
    "Why was 6 afraid of 7? Because 7 ate 9.";
    "How many tickles can an octopus take? Tentacles!";
    "What did the Atlantic Ocean say to the Pacific Ocean? Nothing, it just waved.";
    "How do you make holy water? You boil the hell out of it.";
    |]
  in
    Random.self_init () ;
    let idx = Random.int 5 in
    print_endline arr_of_jokes.(idx)
