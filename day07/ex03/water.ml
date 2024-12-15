class water =
  object
    inherit Molecule.molecule "Water" [(2, new Hydrogen.hydrogen); (1, new Oxygen.oxygen)]
  end
