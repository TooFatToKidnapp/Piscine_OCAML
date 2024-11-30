let my_sleep () = Unix.sleep 1

let () =
  if Array.length Sys.argv <> 2 then begin
    print_string "Usage: ";
    print_string Sys.argv.(0);
    print_endline " <count>";
  end
  else
    match int_of_string_opt (Array.get Sys.argv 1) with
    | None ->
      print_string "Error: Invalid number [";
      print_string (Array.get Sys.argv 1);
      print_endline "]";
    | Some count ->
        if count > 0 then
          for i = 1 to count do
            my_sleep ()
          done
        else
          print_endline "Error: Count must be greater than 0"
