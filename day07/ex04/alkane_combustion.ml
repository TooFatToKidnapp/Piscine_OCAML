class alkane_combustion (alkanes: Alkane.alkane list) =
  object (this)
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
end
