type phosphate = string

type deoxyribose = string

type nucleobase =
    | A
    | T
    | C
    | G
    | None

type nucleotide = {
  phos: phosphate;
  deoxy: deoxyribose;
  nucleo : nucleobase
}

let generate_nucleotide c : nucleotide =
  let create_nucleotide base = {
    phos = "phosphate";
    deoxy = "deoxyribose";
    nucleo = base
  }
  in
  match c with
  | 'A' -> create_nucleotide A
  | 'T' -> create_nucleotide T
  | 'C' -> create_nucleotide C
  | 'G' -> create_nucleotide G
  | _ -> create_nucleotide None




let print_nucleotide n =
    let nucleobase_to_str m =
    match m with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | _ -> "None"
  in
    Printf.printf "{\n\t.phos = %s\n\t.deoxy = %s\n\t.nucleo = %s\n}\n" n.phos n.deoxy (nucleobase_to_str n.nucleo)
let () =
  print_nucleotide (generate_nucleotide 'A');
  print_nucleotide (generate_nucleotide 'C');
  print_nucleotide (generate_nucleotide 'T');
  print_nucleotide (generate_nucleotide 'G');
  print_nucleotide (generate_nucleotide 'X')
