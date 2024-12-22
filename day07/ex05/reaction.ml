class virtual reaction (to_react: (Molecule.molecule * int) list) (product: (Molecule.molecule * int) list) =
  object
    val reactives: (Molecule.molecule * int) list = to_react
    val products: (Molecule.molecule * int) list = product
    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool
  end
