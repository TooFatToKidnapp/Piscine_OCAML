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

type helix = nucleotide list

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

let generate_helix n =
  let rec create_helix acc n =
    let random_int_in_range ~min ~max =
      min + (Random.int (max - min + 1))
    in
    match n with
    | 0 -> acc
    | _ ->
        Random.self_init ();
        (match random_int_in_range 0 3 with
        | 0 -> generate_nucleotide 'A'
        | 1 -> generate_nucleotide 'C'
        | 2 -> generate_nucleotide 'G'
        | 3 -> generate_nucleotide 'T'
        | _ -> generate_nucleotide 'X')
        :: create_helix acc (n - 1)
    in
    create_helix [] n

let helix_to_string h =
  let rec aux h =
    match h with
    | [] -> ""
    | h::t -> match h.nucleo with
              | A -> "A" ^ aux t
              | C -> "C" ^ aux t
              | G -> "G" ^ aux t
              | T -> "T" ^ aux t
              | _ -> "" ^ aux t
  in
  aux h

let complementary_helix (h: helix) =
  let rec aux h  =
    match h with
    | [] -> []
    | h::t -> match h.nucleo with
            | A -> (generate_nucleotide 'T') :: aux t
            | C -> (generate_nucleotide 'G') :: aux t
            | G -> (generate_nucleotide 'C') :: aux t
            | T -> (generate_nucleotide 'A') :: aux t
            | _ -> (generate_nucleotide 'X') :: aux t
  in
  aux h

let print_nucleotide n =
    let nucleobase_to_str m =
    match m with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | _ -> "None"
  in
  nucleobase_to_str n

let rec print_helix = function
| [] -> ()
| h::t ->
  Printf.printf "{\n\t.phos = %s\n\t.deoxy = %s\n\t.nucleo = %s\n}\n" h.phos h.deoxy (print_nucleotide h.nucleo);
  print_helix t

let () =
  let h = generate_helix 5 in
  print_helix h;
  print_endline (helix_to_string h);
  print_string "complementary_helix: ";
  print_helix (complementary_helix h);
