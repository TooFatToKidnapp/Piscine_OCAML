type phosphate = string

type deoxyribose = string

type nucleobase =
    | A
    | T
    | C
    | G
    | U
    | None

type nucleotide = {
  phos: phosphate;
  deoxy: deoxyribose;
  nucleo : nucleobase
}

type helix = nucleotide list

type rna = nucleobase list

type aminoacid =
  | Stop
  | Ala
  | Arg
  | Asn
  | Asp
  | Cys
  | Gln
  | Glu
  | Gly
  | His
  | Ile
  | Leu
  | Lys
  | Met
  | Phe
  | Pro
  | Ser
  | Thr
  | Trp
  | Tyr
  | Val

type protein = aminoacid list


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

let generate_helix n  =
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



let generate_rna h =
  let rec creat_rna acc = function
  | [] -> acc
  | h::t -> (match h.nucleo with
            | A -> U
            | C -> G
            | G -> C
            | T -> A
            | _ -> None) :: creat_rna acc t
  in
  creat_rna [] h

let generate_bases_triplets (rna:rna) =
  let rec create_triplets rna_lst output = match rna_lst with
      | h :: a :: b :: tail -> create_triplets tail (output @ [(h, a, b)])
      | _ -> output
  in create_triplets rna []


let string_of_protein (p: protein) =
  let rec build_protien_str acc = function
    | [] -> acc
    | h::t -> match h with
            | Stop -> "End of Translation" ^ acc
            | Ala -> "Alanine , " ^ (build_protien_str acc t)
            | Arg -> "Arginine , " ^ (build_protien_str acc t)
            | Asn -> "Asparagine , " ^ (build_protien_str acc t)
            | Asp -> "Aspartique , " ^ (build_protien_str acc t)
            | Cys -> "Cysteine , " ^ (build_protien_str acc t)
            | Gln -> "Glutamine , " ^ (build_protien_str acc t)
            | Glu -> "Glutamique , " ^ (build_protien_str acc t)
            | Gly -> "Glycine , " ^ (build_protien_str acc t)
            | His -> "Histidine , " ^ (build_protien_str acc t)
            | Ile -> "Isoleucine , " ^ (build_protien_str acc t)
            | Leu -> "Leucine , " ^ (build_protien_str acc t)
            | Lys -> "Lysine , " ^ (build_protien_str acc t)
            | Met -> "Methionine , " ^ (build_protien_str acc t)
            | Phe -> "Phenylalanine , " ^ (build_protien_str acc t)
            | Pro -> "Proline , " ^ (build_protien_str acc t)
            | Ser -> "Serine , " ^ (build_protien_str acc t)
            | Thr -> "Threonine , " ^ (build_protien_str acc t)
            | Trp -> "Tryptophane , " ^ (build_protien_str acc t)
            | Tyr -> "Tyrosine , " ^ (build_protien_str acc t)
            | Val -> "Valine , " ^ (build_protien_str acc t)
in
  build_protien_str "" p
  let rec decode_arn rna =
    let rna_base_tripels = generate_bases_triplets rna in
    let rec aux rna acc =
    match rna with
    | h::t when h = (U,G,G) -> Trp::aux t acc
    | h::t when h = (A,U,G) -> Met::aux t acc
    | h::n1::t when h = (A,A,C) && n1 = (A,A,U) -> Asn::aux t acc
    | h::n1::t when h = (G,A,C) && n1 = (G,A,U) -> Asp::aux t acc
    | h::n1::t when h = (U,G,C) && n1 = (U,G,U) -> Cys::aux t acc
    | h::n1::t when h = (C,A,A) && n1 = (C,A,G) -> Gln::aux t acc
    | h::n1::t when h = (G,A,A) && n1 = (G,A,G) -> Glu::aux t acc
    | h::n1::t when h = (A,A,A) && n1 = (A,A,G) -> Lys::aux t acc
    | h::n1::t when h = (U,U,C) && n1 = (U,U,U) -> Phe::aux t acc
    | h::n1::t when h = (U,A,C) && n1 = (U,A,U) -> Tyr::aux t acc
    | h::n1::t when h = (C,A,C) && n1 = (C,A,U) -> His::aux t acc
    | h::n1::n2::t when h = (U,A,A) && n1 = (U,A,G) && n2 = (U,G,A) -> Stop::aux t acc
    | h::n1::n2::t when h = (A,U,A) && n1 = (A,U,C) && n2 = (A,U,U) -> Ile::aux t acc
    | h::n1::n2::n3::t when h = (G,G,A) && n1 = (G,G,C) && n2 = (G,G,G) && n3 = (G,G,U) -> Gly::aux t acc
    | h::n1::n2::n3::t when h = (A,C,A) && n1 = (A,C,C) && n2 = (A,C,G) && n3 = (A,C,U) -> Thr::aux t acc
    | h::n1::n2::n3::t when h = (C,C,C) && n1 = (C,C,A) && n2 = (C,C,G) && n3 = (C,C,U) -> Pro::aux t acc
    | h::n1::n2::n3::t when h = (G,U,A) && n1 = (G,U,C) && n2 = (G,U,G) && n3 = (G,U,U) -> Val::aux t acc
    | h::n1::n2::n3::t when h = (G,C,A) && n1 = (G,C,C) && n2 = (G,C,G) && n3 = (G,C,U) -> Ala::aux t acc
    | h::n1::n2::n3::n4::n5::t when h = (A,G,A) && n1 = (A,G,G) && n2 = (C,G,A) && n3 = (C,G,C) && n4 = (C,G,G) && n5 = (C,G,U) -> Arg::aux t acc
    | h::n1::n2::n3::n4::n5::t when h = (C,U,A) && n1 = (C,U,C) && n2 = (C,U,G) && n3 = (C,U,U) && n4 = (U,U,A) && n5 = (U,U,G) -> Leu::aux t acc
    | h::n1::n2::n3::n4::n5::t when h = (U,C,A) && n1 = (U,C,C) && n2 = (U,C,G) && n3 = (U,C,U) && n4 = (A,G,U) && n5 = (A,G,C) -> Ser::aux t acc
    | _ -> []
    in
    aux rna_base_tripels []

let rec print_base_triples lst =
let as_str = function
  | A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | U -> "U"
  | _ -> "None"
  in
  match lst with
| [] -> Printf.printf "\n"
| h::t -> match h with
| (a, b, c) -> begin
  Printf.printf "[%s%s%s]" (as_str a) (as_str b) (as_str c);
  print_base_triples t
end


let () =
  let rna = generate_rna (generate_helix 12) in
  let base_tripl = (generate_bases_triplets rna) in
  print_base_triples base_tripl;




