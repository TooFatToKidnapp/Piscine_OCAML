let () =
  if Array.length Sys.argv <> 2 then
    Printf.printf "Usage: %s <jokes_file_path>\n" Sys.argv.(0)
  else begin
    let jokes_file_path = Sys.argv.(1) in
    let open_file filename =
      try
        let channel = open_in filename in
        Some channel
      with
      | Sys_error _ -> None
    in
    match open_file jokes_file_path with
    | None -> Printf.printf "Error: [%s] no sutch file\n" jokes_file_path
    | Some file_handler -> begin
      let jokes_arr = ref [||] in
      try
        while true do
          let line = input_line file_handler in
          if line <> "" then jokes_arr := Array.append !jokes_arr [|line|]
        done
      with End_of_file -> close_in file_handler;
      if Array.length !jokes_arr = 0 then
        Printf.printf "Error: empty jokes file\n"
      else begin
        Random.self_init () ;
        let idx = Random.int (Array.length !jokes_arr) in
        print_endline !jokes_arr.(idx);
      end
    end
  end
