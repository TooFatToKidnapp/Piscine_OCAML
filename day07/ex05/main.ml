(* let rec print_alkanes = function
| []  -> ()
| head::tail  ->
  print_string (head#get_formula);
  print_alkanes tail *)

let () =
  let alkanes = [ new Ethane.ethane; ] in
  (* Printf.printf "Alcane start = ";
  print_alkanes alkanes;
  print_char '\n'; *)
  let combustion = new Alkane_combustion.alkane_combustion alkanes in
  let incomplete_results = combustion#get_incomplete_results [((new Carbon.carbon), 2); (new Hydrogen.hydrogen, 6); (new Oxygen.oxygen, 6);]in

  List.iter (fun (missing_oxygen, products) ->
    Printf.printf "Missing oxygen: %d\n" missing_oxygen;
    List.iter (fun (product, amount) ->
      Printf.printf "  %s: %d\n" (product#get_formula) amount
    ) products
  ) incomplete_results;
