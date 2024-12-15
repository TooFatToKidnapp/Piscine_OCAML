class virtual atom (atom_name: string) (atom_symbol: string) (atom_atomic_number: int) =
  object(self)
    val name = atom_name
    val symbol = atom_symbol
    val atomic_number = atom_atomic_number

    method name = name
    method symbol = symbol
    method atomic_number = atomic_number
    method to_string =
      "atom -> name: " ^ self#name ^ " | symbol: " ^ self#symbol ^ " | atomic_number: " ^ (string_of_int self#atomic_number)
    method equals (a: atom) =
      (self#name = a#name) && (self#symbol = a#symbol) && (self#atomic_number = a#atomic_number)
  end
