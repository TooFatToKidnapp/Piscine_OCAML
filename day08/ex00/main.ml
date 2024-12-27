(* Monoid TLDR *)
(* Must inclose a set under a binary operator. meaning 2 elements of a set map to another element in the same set *)
(* The binary operator must be associative. meaning order of operation dosent matter *)
(* Must have a neutral set element *)

let () =
  let current_time = 6 in
  Printf.printf "current time is = %d\n" current_time;
  Printf.printf "new time after 7 hours = %d\n" (Clock.Watchtower.add current_time 7);
  let new_time = Clock.Watchtower.add current_time 7 in
  Printf.printf "new time decremented by 3 hours = %d\n" (Clock.Watchtower.sub new_time 3);

