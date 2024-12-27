module type SET = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val union: 'a t -> 'a t -> 'a t
  val inter: 'a t -> 'a t -> 'a t
  val diff: 'a t -> 'a t -> 'a t
  val filter: 'a t -> ('a -> bool) -> 'a t
  val foreach: 'a t -> ('a -> unit) -> unit
  val for_all: 'a t -> ('a -> bool) -> bool
  val existes: 'a t -> ('a -> bool) -> bool

end

module Set: SET = struct
  type 'a t = 'a list
  let return e = [e]
  let bind s fn = List.concat (List.map fn s)
  let union s1 s2 =
    let merge_lists = s1 @ s2 in
      let rec iter acc s =
        match s with
        | [] -> acc
        | h :: t when (List.find_opt (fun x -> x = h) acc ) = None -> iter (acc @ [h]) t
        | _ :: t -> iter acc t
      in
    iter [] merge_lists

  let inter s1 s2 =
    List.filter (fun x -> if List.find_opt (fun y -> y = x ) s2 = None then false else true) s1

  let diff s1 s2 =
    let not_in_s1 = List.filter (fun x -> if List.find_opt (fun y -> y = x ) s2 = None then true else false) s1 in
    let not_in_s2 = List.filter (fun x -> if List.find_opt (fun y -> y = x ) s1 = None then true else false) s2 in
    not_in_s1 @ not_in_s2

  let filter s fn = List.filter fn s
  let foreach s fn = List.iter fn s
  let for_all s fn = List.for_all fn s
  let existes s fn = List.exists fn s

end
