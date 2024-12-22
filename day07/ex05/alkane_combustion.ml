let print_molecules (molecules: (Molecule.molecule * int) list) =
  List.iter (fun (molecule, amount) ->
    let formula = molecule#get_formula in
    Printf.printf "%d %s\n" amount formula
  ) molecules
;;
class alkane_combustion (to_react: Alkane.alkane list) =
  let rec alkane_molecule_count alkanes c h =
    match alkanes with
    | [] -> (c ,h)
    | head::t -> alkane_molecule_count t (head#carbon_count + c) (head#hydrogen_count + h)
  in
  let rec reaction_start (alkanes: Molecule.molecule list)  acc =
    match alkanes with
    | [] -> acc
    | h::t -> match acc with
              | (m, c) :: n when h#equals m -> reaction_start t ((m ,(c + 1)) :: n)
              | _ -> reaction_start t ((h, 1) :: acc)
  in
  let ppcm a b =
      let rec iter acc_a acc_b =
        let b' = acc_b * b in
        let a' = acc_a * a in
          if a' = b' then a'
          else if a' < b' then iter (acc_a + 1) acc_b
          else iter acc_a (acc_b + 1)
        in
        if a = 0 || b = 0 then 0
        else iter 1 1
  in
  object
  val start: ((Molecule.molecule * int) list) = reaction_start (to_react :> Molecule.molecule list) [(new Dioxygen.dioxygen, 0)]
  val result: ((Molecule.molecule * int) list) = []
  val init_molecule_count = alkane_molecule_count to_react 0 0
  val balanced = false
  inherit Reaction.reaction [] []
  method is_balanced = balanced
  method get_start = if balanced then start else failwith "Not balanced"
  method get_result = if balanced then result else failwith "Not balanced"
  method balance =
    let balance_left f diox =
        let rec iter = function
        | [] -> []
        | (m, _) :: t when m#get_formula = "O2" -> (m, diox) :: (iter t)
        | (m, n) :: t -> (m, n * f) :: (iter t)
    in
    iter start
    and balance_right cd_count w_count =
      [(new Carbon_dioxyde.carbon_dioxyde, cd_count); (new Water.water, w_count)]
    in
      let (c, h) = init_molecule_count in
      if c > 0 && h > 0 then
        let dcarbon = ppcm 4 c in
        let fact = dcarbon / c in
        let water = (h * fact) / 2 in
        let diox = (2 * dcarbon + water) / 2 in
        ({< start = balance_left fact diox; result = balance_right dcarbon water; balanced = true >} :> Reaction.reaction)
      else
        ({< balanced = true>} :> Reaction.reaction)

        method get_incomplete_results molecule_list =
          let rec compute_results oxygen_amount molecules acc =
            match molecules with
            | [] -> acc
            | (head, amount)::tail ->
               let (carbon, hydrogen) = (head#carbon_count, head#hydrogen_count) in
               let oxygen_needed = (2 * carbon + (hydrogen / 2)) * amount in
               if oxygen_needed <= oxygen_amount then
                 compute_results (oxygen_amount - oxygen_needed) tail acc
               else
                 let missing_oxygen = oxygen_needed - oxygen_amount in
                 let products =
                   [
                     (new Carbon_monoxide.carbon_monoxide, carbon * amount);
                     (new Soot.soot, carbon * amount);
                     (new Water.water, (hydrogen * amount) / 2);
                   ]
                 in
                 compute_results oxygen_amount tail ((missing_oxygen, products) :: acc)
          in
          let (carbon, hydrogen) = init_molecule_count in
          let max_oxygen = 2 * carbon + (hydrogen / 2) in
          print_molecules molecule_list;
          List.rev (compute_results max_oxygen molecule_list [])
  end
