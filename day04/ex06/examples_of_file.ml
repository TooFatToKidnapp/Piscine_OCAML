let examples_of_file file_path : (float array * string) list =
  let file_path_len = String.length file_path in
  if file_path_len >= 4 && String.sub file_path (file_path_len - 4) 4 = ".csv" then begin
    let open_file filename =
      try Ok (open_in filename)
      with Sys_error err -> Error err
    in
    match open_file file_path with
    | Error err ->
        Printf.printf "Error: Can't open file [%s]\n%s\n" file_path err;
        []
    | Ok file_handler ->
        let rec read_lines acc =
          try
            let line = input_line file_handler in
            let line_split = String.split_on_char ',' line in
            match List.rev line_split with
            | [] -> read_lines acc
            | label :: features when (String.length label > 0 && (label.[0] = 'g' || label.[0] = 'd')) -> (
                try
                  let float_list = List.map float_of_string (List.rev features) in
                  let float_arr = Array.of_list float_list in
                  read_lines ((float_arr, label) :: acc)
                with Failure _ -> read_lines acc
              )
            | _ -> read_lines acc
          with End_of_file ->
            close_in file_handler;
            List.rev acc
        in
        read_lines []
  end else begin
    Printf.printf "Error: not a valid CSV file\n";
    []
  end


let print_examples examples =
  List.iter (fun (floats, label) ->
    Array.iter (Printf.printf "%f ") floats;
    Printf.printf " -> %s\n" label
  ) examples

let () =
  print_examples (examples_of_file "ionosphere.test.csv");
