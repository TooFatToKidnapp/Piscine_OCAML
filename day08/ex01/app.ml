module type APP = sig

  type project = (string * string * int)

  val zero: (string * string * int)

  val combine: project -> project -> project

  val fail: project -> project

  val success: project -> project

end


module App = struct

  type project = string * string * int

  let zero = ("" , "", 0)

  let combine (lhs: project) (rhs: project) : project =
    match (lhs, rhs) with
    | ((p1, _, g1), (p2, _, g2)) -> begin
      let av = (g1 + g2) / 2 in
      if av > 80 then (p1^p2, "succeed", av)
      else (p1^p2, "failed", av)
    end

  let fail (p: project) : project =
    match p with
    | (p, _, _) -> (p, "failed", 0)

  let success (p: project) : project =
    match p with
    | (p, _, _) -> (p, "succeed", 80)


end
