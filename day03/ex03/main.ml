let () =
  let deck = Deck.newDeck () in

  Printf.printf "Deck1: %s\n" (String.concat ", " (Deck.toStringList deck));

  let deck2 = Deck.newDeck () in

  Printf.printf "Deck2: %s\n" (String.concat ", " (Deck.toStringList deck2));

  let (card, remaining_deck) = Deck.drawCard deck in

  Printf.printf "Drawn card in deck1: %s\n" (Deck.Card.toString card);

  Printf.printf "Remaining deck1: %s\n" (String.concat ", " (Deck.toStringList remaining_deck));

  let (card2, remaining_deck2) = Deck.drawCard remaining_deck in

  Printf.printf "Second drawn card in deck1: %s\n" (Deck.Card.toString card2);

  Printf.printf "Remaining deck after second draw: %s\n" (String.concat ", " (Deck.toStringListVerbose remaining_deck2));
