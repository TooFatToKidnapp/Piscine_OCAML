class carbon_monoxide =
  object
    inherit Molecule.molecule "Carbon_monoxide" [(1, new Oxygen.oxygen); (1, new Carbon.carbon)]
  end
