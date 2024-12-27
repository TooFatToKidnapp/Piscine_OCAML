class water =
  object
    inherit Molecule.molecule "Water" [(1, new Oxygen.oxygen); (2, new Hydrogen.hydrogen)]
  end
