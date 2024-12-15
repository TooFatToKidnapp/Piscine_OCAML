class virtual reaction (to_react: (int * Molecule.molecule ) list) (product: (int * Molecule.molecule ) list) =
  object
    val reactives: (int* Molecule.molecule) list = to_react
    val products: (int* Molecule.molecule) list = product
    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool
  end
