let rec print_molecules = function
| [] -> ()
| (m,n)::tail ->
  print_char ' ';
  print_int n;
  print_char '(';
  print_string (m#get_formula);
  print_char ')';
  print_molecules tail

let rec print_alkanes = function
| []  -> ()
| head::tail  ->
  print_string (head#get_formula);
  print_alkanes tail

let test start =
  print_alkanes start;
  let r = (new Alkane_combustion.alkane_combustion start) in
  print_string (if r#is_balanced then " (balanced)" else " (unbalanced)");
  print_string " ----> ";
  let r = r#balance in
  print_molecules (r#get_start);
  print_string " = ";
  print_molecules (r#get_result);
  print_string (if r#is_balanced then " (balanced)" else " (unbalanced)");
  print_newline ()

let () =
  let alkanes = [ new Ethane.ethane; ] in
  let combustion = new Alkane_combustion.alkane_combustion alkanes in
  let incomplete_results = combustion#get_incomplete_results in
  test alkanes;
  List.iter (fun (missing_oxygen, products) ->
    Printf.printf "Missing oxygen: %d\n" missing_oxygen;
    List.iter (fun (product, amount) ->
      Printf.printf "  %s: %d\n" (product#get_formula) amount
    ) products
  ) incomplete_results;
