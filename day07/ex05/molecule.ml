class virtual molecule (m_name: string) (atom_list: (int * Atom.atom) list) =
  let build_molecule_formula (atom_lst: (int * Atom.atom) list) =
    let sort_by_name (x: (int * Atom.atom)) (y: (int * Atom.atom)) =
      match (x, y) with
      | ((_, xa), (_, ya)) -> String.compare xa#symbol ya#symbol
    in
    let sort_by_name = List.sort sort_by_name atom_lst in
    let rec aux (acc1: string) (acc2: string) lst =
      match lst with
      | [] -> (acc1 ^ acc2)
      | (count, a) :: t ->
        let mol_count = if count <> 1 then string_of_int count else "" in
        if String.equal a#symbol "C" then aux ((a#symbol ^ mol_count) ^ acc1) acc2 t
        else if String.equal a#symbol "H" then aux (acc1 ^ (a#symbol ^ mol_count)) acc2 t
        else aux acc1 (acc2 ^ (a#symbol ^ mol_count)) t
      in
      aux "" "" sort_by_name
  in
  let count_atom c =
    let rec aux acc = function
    | [] -> acc
    | (count, h) :: t -> if h#symbol = c then aux (acc + count) t else aux acc t
  in
  aux 0 atom_list
in
  object(self)
    val name = m_name
    val formula = build_molecule_formula atom_list
    method get_name = name
    method get_formula = formula
    method to_string =
      "molecule -> " ^ "name: " ^ self#get_name ^ " | formula: " ^ self#get_formula
    method equals (m:molecule) =
      (self#get_name = m#get_name) && (self#get_formula = m#get_formula)
    method carbon_count =
      count_atom "C"
    method hydrogen_count =
      count_atom "H"
    end
