type t =
  | Spade
  | Heart
  | Diamond
  | Club
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
