let () =
  let test_values label f =
    Printf.printf "%s:\n" label;
    List.iter (fun v -> Printf.printf "%s " (f v)) Value.all;
    Printf.printf "\n\n"
  in

  test_values "toString" Value.toString;
  test_values "toStringVerbose" Value.toStringVerbose;

  Printf.printf "toInt:\n";
  List.iter (fun v -> Printf.printf "%d " (Value.toInt v)) Value.all;
  Printf.printf "\n\n";

  Printf.printf "next:\n";
  List.iter
    (fun v ->
      try Printf.printf "%s -> %s\n" (Value.toString v) (Value.toString (Value.next v))
      with Invalid_argument _ -> Printf.printf "%s -> None\n" (Value.toString v))
    Value.all;

  Printf.printf "\nprevious:\n";
  List.iter
    (fun v ->
      try Printf.printf "%s -> %s\n" (Value.toString v) (Value.toString (Value.previous v))
      with Invalid_argument _ -> Printf.printf "%s -> None\n" (Value.toString v))
    Value.all;

