let () =
  let current_time = 6 in
  Printf.printf "current time is = %d\n" current_time;
  Printf.printf "new time after 7 hours = %d\n" (Clock.Watchtower.add current_time 7);
  let new_time = Clock.Watchtower.add current_time 7 in
  Printf.printf "new time decremented by 3 hours = %d\n" (Clock.Watchtower.sub new_time 3);

