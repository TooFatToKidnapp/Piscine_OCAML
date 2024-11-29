module Color = struct
  type t = Spade | Heart | Diamond | Club
  let all = [Spade; Heart; Diamond; Club]
  let toString = function
    | Club -> "C"
    | Heart -> "H"
    | Diamond -> "D"
    | Spade -> "S"
  let toStringVerbose = function
    | Club -> "Club"
    | Heart -> "Heart"
    | Diamond -> "Diamond"
    | Spade -> "Spade"
end

module Value = struct
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
  let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]
  let toInt = function
    | T2 -> 1
    | T3 -> 2
    | T4 -> 3
    | T5 -> 4
    | T6 -> 5
    | T7 -> 6
    | T8 -> 7
    | T9 -> 8
    | T10 -> 9
    | Jack -> 10
    | Queen -> 11
    | King -> 12
    | As -> 13
  let toString = function
    | T2 -> "1"
    | T3 -> "2"
    | T4 -> "3"
    | T5 -> "4"
    | T6 -> "5"
    | T7 -> "6"
    | T8 -> "7"
    | T9 -> "8"
    | T10 -> "9"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"
  let toStringVerbose = function
    | T2 -> "1"
    | T3 -> "2"
    | T4 -> "3"
    | T5 -> "4"
    | T6 -> "5"
    | T7 -> "6"
    | T8 -> "7"
    | T9 -> "8"
    | T10 -> "9"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | As -> "As"
  let next = function
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> As
    | As -> invalid_arg "As has no next value"
  let previous = function
    | T2 -> invalid_arg "T2 has no previous value"
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen -> Jack
    | King -> Queen
    | As -> King
end

type t = {
  value: Value.t;
  color: Color.t;
}

let newCard value color = {value; color}

let allSpades =
  let rec create_spades_list (spade_values: Value.t list) (acc: t list) =
    match spade_values with
    | [] -> acc
    | h::t -> (newCard h Color.Spade ) :: create_spades_list t acc
  in
  create_spades_list Value.all []

let allHearts =
  let rec create_hearts_list (heart_values: Value.t list) (acc: t list) =
    match heart_values with
    | [] -> acc
    | h::t -> (newCard h Color.Heart) :: create_hearts_list t acc
  in
  create_hearts_list Value.all []

let allDiamonds =
  let rec create_diamonds_list (diamond_values: Value.t list) (acc: t list) =
    match diamond_values with
    | [] -> acc
    | h::t -> (newCard h Color.Diamond) :: create_diamonds_list t acc
  in
  create_diamonds_list Value.all []

let allClubs =
  let rec create_clubs_list (club_values: Value.t list) (acc: t list) =
    match club_values with
    | [] -> acc
    | h::t -> (newCard h Color.Club) :: create_clubs_list t acc
  in
  create_clubs_list Value.all []

let all = allClubs @ allDiamonds @ allHearts @ allSpades

let getValue card =
  card.value

let getColor card =
  card.color

let toString card =
  Value.toString card.value ^ Color.toString card.color

let toStringVerbose card =
  "Card(" ^ (Value.toStringVerbose card.value) ^ ", " ^ (Color.toStringVerbose card.color) ^ ")"

let compare card1 card2 =
  Value.toInt card1.value - Value.toInt card2.value

let max card1 card2 =
  if Value.toInt card2.value > Value.toInt card1.value then card2
  else card1

let min card1 card2 =
  if Value.toInt card2.value < Value.toInt card1.value then card2
  else card1

let best card_list =
  match card_list with
  | [] -> invalid_arg "Empty List"
  | h::t -> List.fold_left max h t

let isOf card clr =
  card.color = clr

let isSpade card =
  card.color = Color.Spade

let isHeart card =
  card.color = Color.Heart

let isDiamond card =
  card.color = Color.Diamond

let isClub card =
  card.color = Color.Club
