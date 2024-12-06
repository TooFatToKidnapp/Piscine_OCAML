module type FIXED = sig
  type t
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool (** physical equality *)
  val eqs : t -> t -> bool (** structural equality *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig
  val bits: int
end

module type MAKE = functor (Input: FRACTIONNAL_BITS) -> FIXED

module Make: MAKE = functor (Input: FRACTIONNAL_BITS) ->
  struct
  let bits = Input.bits
  type t = int
  let to_int (t: t) = t lsr bits
  let of_int (i: int): t = i lsl bits
  let to_float (t: t) = float_of_int t /. float_of_int (of_int 1)
  let of_float (f: float) =
    int_of_float (floor (0.5 +.(f *. (float_of_int (of_int 1)))))
  let to_string (t: t) =
    let tfl = to_float t in
    let tint = to_int t in
    if tfl = 0. || tfl /. float_of_int tint = 1.0 then
      string_of_int tint
    else string_of_float tfl

  let zero = of_int 0
  let one = of_int 1
  let succ t = t + 1
  let pred t = t - 1
  let min t1 t2 = if t1 <= t2 then t1 else t2
  let max t1 t2 = if t1 >= t2 then t1 else t2
  let gth t1 t2 = if t1 > t2 then true else false
  let lth t1 t2 = if t1 < t2 then true else false
  let gte t1 t2 = if t1 >= t2 then true else false
  let lte t1 t2 = if t1 <= t2 then true else false
  let eqp t1 t2 = if t1 == t2 then true else false
  let eqs t1 t2 = if t1 = t2 then true else false
  let add t1 t2 = t1 + t2
  let sub t1 t2 = t1 - t2
  let mul t1 t2 = t1 * t2
  let div t1 t2 = t1 / t2

  let rec foreach st ed f =
    match st with
    | x when eqs x ed -> f st
    | _ -> f st; foreach (succ st) ed f

end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))
