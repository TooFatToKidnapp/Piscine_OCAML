module type WATCHTOWER = sig
  type hour = int
  val zero : hour
  val add : hour -> hour -> hour
  val sub : hour -> hour -> hour
end

module Watchtower: WATCHTOWER = struct
  type hour = int (* Monoid set *)
  let zero = 12 (* identity element *)

  (* Monoid binary operations set -> set -> set *)
  let add h c = (h + (c mod zero)) mod zero
  let sub h c = let f = if h < (c mod zero) then 12 else 0 in
                f + (h - (c mod zero))
end
