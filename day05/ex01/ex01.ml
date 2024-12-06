module StringHashtbl = Hashtbl.Make(struct
  type t = string
  let equal (t1: t)  (t2: t) =
    t1 = t2
  let hash s =
    let rec aux index acc =
      match index with
      | len when len = String.length s -> acc
      | _ -> aux (index + 1) (acc + index + int_of_char s.[index])
    in
    aux 0 0

end)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
