let () =
  let open Card in
  let card1 = newCard Value.T10 Color.Spade in
  let card2 = newCard Value.King Color.Heart in
  let card3 = newCard Value.As Color.Diamond in
  let card4 = newCard Value.T2 Color.Club in
  let card_list = [card1; card2; card3; card4] in

  Printf.printf "Card 1: %s\n" (toStringVerbose card1);
  Printf.printf "Card 2: %s\n" (toStringVerbose card2);
  Printf.printf "Card 3: %s\n" (toStringVerbose card3);
  Printf.printf "Card 4: %s\n" (toStringVerbose card4);

  Printf.printf "Max of card1 and card2: %s\n" (toStringVerbose (max card1 card2));
  Printf.printf "Min of card1 and card2: %s\n" (toStringVerbose (min card1 card2));

  Printf.printf "Best card: %s\n" (toStringVerbose (List.fold_left max (List.hd card_list) (List.tl card_list)));

  Printf.printf "Is card1 a Spade? %b\n" (isSpade card1);
  Printf.printf "Is card2 a Heart? %b\n" (isHeart card2);
  Printf.printf "Is card3 a Diamond? %b\n" (isDiamond card3);
  Printf.printf "Is card4 a Club? %b\n" (isClub card4);

  Printf.printf "Value of card1: %s\n" (Value.toString (getValue card1));
  Printf.printf "Color of card1: %s\n" (Color.toString (getColor card1));

  Printf.printf "All cards: %s\n" (String.concat ", " (List.map toStringVerbose all))
