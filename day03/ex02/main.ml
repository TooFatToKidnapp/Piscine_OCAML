let () =
  let card1 = Card.newCard  Card.Value.T10  Card.Color.Spade in
  let card2 = Card.newCard  Card.Value.King  Card.Color.Heart in
  let card3 = Card.newCard  Card.Value.As  Card.Color.Diamond in
  let card4 = Card.newCard  Card.Value.T2  Card.Color.Club in
  let card_list = [card1; card2; card3; card4] in

  Printf.printf "Card 1: %s\n" (Card.toStringVerbose card1);
  Printf.printf "Card 2: %s\n" (Card.toStringVerbose card2);
  Printf.printf "Card 3: %s\n" (Card.toStringVerbose card3);
  Printf.printf "Card 4: %s\n" (Card.toStringVerbose card4);

  Printf.printf "Max of card1 and card2: %s\n" (Card.toStringVerbose (max card1 card2));
  Printf.printf "Min of card1 and card2: %s\n" (Card.toStringVerbose (min card1 card2));

  Printf.printf "Best card: %s\n" (Card.toStringVerbose (List.fold_left max (List.hd card_list) (List.tl card_list)));

  Printf.printf "Is card1 a Spade? %b\n" (Card.isSpade card1);
  Printf.printf "Is card2 a Heart? %b\n" (Card.isHeart card2);
  Printf.printf "Is card3 a Diamond? %b\n" (Card.isDiamond card3);
  Printf.printf "Is card4 a Club? %b\n" (Card.isClub card4);

  Printf.printf "Value of card1: %s\n" ( Card.Value.toString (Card.getValue card1));
  Printf.printf "Color of card1: %s\n" ( Card.Color.toString (Card.getColor card1));

  Printf.printf "All cards: %s\n" (String.concat ", " (List.map Card.toStringVerbose  Card.all))
