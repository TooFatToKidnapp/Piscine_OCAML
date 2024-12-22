let print_proj (p: App.App.project): unit =
  match p with
  | (p, s, g) ->
    Printf.printf "project -> %s | status -> %s | grade -> %d\n" p s g

let () =
  let p = ("Game 1" , "failed", 10) in
  print_proj p;
  print_proj (App.App.success p);
