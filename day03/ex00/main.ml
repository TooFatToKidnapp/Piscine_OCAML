let () =
  let color = Color.Spade in
  let color_str = Color.toString color in
  Printf.printf "Color to string: %s\n" color_str;

  let all_colors = Color.all in
  List.iter (fun c -> Printf.printf "Color to full string: %s\n" (Color.toStringVerbose c)) all_colors
