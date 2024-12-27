class virtual alkane n =
  let get_alkane_name n = match n with
    | 1 -> "Methane"
    | 2 -> "Ethane"
    | 3 -> "Propane"
    | 4 -> "Butane"
    | 5 -> "Pentane"
    | 6 -> "Hexane"
    | 7 -> "Heptane"
    | 8 -> "Octane"
    | 9 -> "Nonane"
    | 10 -> "Decane"
    | 11 -> "Undecane"
    | 12 -> "Dodecane"
    | _ -> failwith "Error: not required to test with n > 12"
  in
  object
    inherit Molecule.molecule (get_alkane_name n) ((n, new Carbon.carbon) :: ((n * 2 + 2), new Hydrogen.hydrogen) :: [])
    method carbon_count = n
    (* method hydrogen_count = (n * 2) + 2 *)
  end

