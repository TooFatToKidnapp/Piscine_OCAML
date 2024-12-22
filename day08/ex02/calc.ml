module type MONOID = sig
  type element
  val zero1 : element
  val zero2 : element
  val mul : element -> element -> element
  val add : element -> element -> element
  val div : element -> element -> element
  val sub : element -> element -> element
end

module INT : (MONOID with type element = int) = struct
  type element = int
  let zero1 = 0
  let zero2 = 1
  let mul lhs rhs = lhs * rhs
  let add lhs rhs = lhs + rhs
  let div lhs rhs = lhs / rhs
  let sub lhs rhs = lhs - rhs

end


module FLOAT :  (MONOID with type element = float) = struct
  type element = float
  let zero1 = 0.
  let zero2 = 1.
  let mul rhs lhs = rhs *. lhs
  let add rhs lhs = rhs +. lhs
  let div rhs lhs = rhs /. lhs
  let sub rhs lhs = rhs -. lhs

end


module type CALC = functor (M : MONOID) -> sig
  val add : M.element -> M.element -> M.element
  val sub : M.element -> M.element -> M.element
  val mul : M.element -> M.element -> M.element
  val div : M.element -> M.element -> M.element
  val power : M.element -> int -> M.element
  val fact : M.element -> M.element

end

module Calc : CALC = functor (M: MONOID) -> struct
  let add rhs lhs = M.add rhs lhs
  let sub rhs lhs = M.sub rhs lhs
  let mul rhs lhs = M.mul rhs lhs
  let div rhs lhs = M.div rhs lhs
  let power rhs lhs =
    let rec aux acc x n =
      if n = 0 then acc
      else aux (M.mul acc x) x (n - 1)
    in
    aux M.zero2 rhs lhs
  let fact x =
    let rec aux acc x =
      if x = M.zero1 then acc
      else aux (M.mul acc x) (M.sub x M.zero2)
    in
    aux M.zero2 x
end
