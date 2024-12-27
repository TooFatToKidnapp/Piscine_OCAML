class alkane_combustion (alkanes: Alkane.alkane list) =
  let rec alkane_molecule_count alkanes c h =
    match alkanes with
    | [] -> (c ,h)
    | head::t -> alkane_molecule_count t (head#carbon_count + c) (head#hydrogen_count + h)
  in

  object (this)
    val init_molecule_count = alkane_molecule_count alkanes 0 0
    val _alkanes = alkanes
    method private format_alkanes: (Molecule.molecule * int) list =
      let rec add_list elem list result =
        match list with
        | [] -> result @ [(elem, 1)]
        | (data, count)::tail ->
           if (data#equals elem) then result @ [(elem, count + 1)] @ tail
           else add_list elem tail (result @ [(data, count)])
      in
      let rec store list result =
        match list with
        | [] -> result
        | head::tail -> store tail (add_list (head :> Molecule.molecule) result [])
      in
      store _alkanes []
    method get_start: (Molecule.molecule * int) list =
      if (this#is_balanced) then
        let rec aux list carbon hydrogen =
          match list with
          | [] -> carbon + hydrogen / 4
          | head::tail -> aux tail (carbon + head#carbon_count) (hydrogen + head#carbon_count * 2 + 2)
      in
			this#format_alkanes @ [(new Dioxygen.dioxygen, (aux _alkanes 0 0))]
    else
      raise (Invalid_argument "Reaction not balanced")
  method get_result: (Molecule.molecule * int) list =
    if (this#is_balanced) then
      let rec aux list carbon hydrogen =
        match list with
        | [] ->
           (carbon, hydrogen)
        | head::tail ->
          aux tail (carbon + head#carbon_count) (hydrogen + head#carbon_count * 2 + 2)
      in
      match (aux _alkanes 0 0) with
      | (c, h) ->
         [(new Carbon_dioxyde.carbon_dioxyde, c)] @ [(new Water.water, h / 2)]
    else
      raise (Invalid_argument "Reaction not balanced")
  method balance =
    if (this#is_balanced) then
      {<_alkanes = alkanes>}
    else
      let smallest lst =
        let rec inner lst smallest =
          match lst with
          | [] -> smallest
          | head::tail ->
             if (((head#carbon_count * 2 + 2) mod 4) = 0) then
               inner tail smallest
             else
               inner tail (min head#carbon_count smallest)
        in
        match _alkanes with
        | [] -> 0
        | _ ->
           inner lst 4000000
      in
      let alkane = smallest _alkanes in
      {<_alkanes = _alkanes @ [object inherit Alkane.alkane alkane end]>}  method is_balanced: bool =
    let rec inner lst hydrogen =
      match lst with
      | [] -> hydrogen
      | head::tail -> inner tail (hydrogen + head#carbon_count * 2 + 2)
    in
    if (((inner _alkanes 0) mod 4) = 0) then
      true
    else
      false

  method private get_oxygen_needed molecule amount =
    let (carbon, hydrogen) = (molecule#carbon_count, molecule#hydrogen_count) in
    (2 * carbon + (hydrogen / 2)) * amount

  method private create_incomplete_products molecule amount oxygen_available =
    let carbon = molecule#carbon_count * amount in
    let hydrogen = molecule#hydrogen_count * amount in
    let oxygen_for_co = min oxygen_available carbon in
    let oxygen_remaining = oxygen_available - oxygen_for_co in
    let oxygen_for_water = min oxygen_remaining (hydrogen / 2) in
    let soot = carbon - oxygen_for_co in
    [
      (new Carbon_monoxide.carbon_monoxide, oxygen_for_co);
      (new Soot.soot, soot);
      (new Water.water, oxygen_for_water)
    ]

  method get_incomplete_results =
    if List.length _alkanes = 0 then
      raise (Invalid_argument "No alkanes provided")
    else
      let total_oxygen_needed = List.fold_left
        (fun acc alkane -> acc + this#get_oxygen_needed alkane 1)
        0 _alkanes in
      let rec compute_all_possibilities oxygen_amount acc =
        if oxygen_amount > total_oxygen_needed then acc
        else
          let rec distribute_oxygen molecules available_oxygen current_result =
            match molecules with
            | [] -> (available_oxygen, current_result)
            | (molecule, amount)::rest ->
                let needed = this#get_oxygen_needed molecule amount in
                if needed <= available_oxygen then
                  distribute_oxygen rest (available_oxygen - needed)
                    (current_result @ this#create_incomplete_products molecule amount needed)
                else
                  let products = this#create_incomplete_products molecule amount available_oxygen in
                  (0, current_result @ products)
          in
          let (_remaining, products) = distribute_oxygen
            (List.map (fun m -> (m, 1)) _alkanes)
            oxygen_amount [] in
          compute_all_possibilities (oxygen_amount + 1)
            ((total_oxygen_needed - oxygen_amount, products) :: acc)
      in
      List.rev (compute_all_possibilities 0 [])

end
