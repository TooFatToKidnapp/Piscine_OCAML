let eu_dist arr1 arr2 =
  if Array.length arr1 <> Array.length arr2 then 0.0
  else begin
    let arr_diff = Array.map2 (fun a b -> a -. b) arr1 arr2 in
    let arr_to_sum = Array.map (fun x -> x *. x) arr_diff in
    let total_sum = Array.fold_left (fun acc v -> acc +. v) 0.0 arr_to_sum in
    sqrt total_sum
  end

let () =
  let test_cases = [
    ([|1.0; 2.0; 3.0|], [|4.0; 5.0; 6.0|], sqrt 27.0);
    ([|0.0; 0.0; 0.0|], [|0.0; 0.0; 0.0|], 0.0);
    ([|1.0; 2.0; 3.0|], [|1.0; 2.0; 3.0|], 0.0);
    ([|1.0; 2.0|], [|1.0; 2.0; 3.0|], 0.0);
    ([|1.0; 2.0; 3.0|], [|1.0; 2.0|], 0.0)
  ] in
  List.iter (fun (arr1, arr2, expected) ->
    let result = eu_dist arr1 arr2 in
    Printf.printf "eu_dist %s %s = %f (expected %f)\n"
      (String.concat "; " (List.map string_of_float (Array.to_list arr1)))
      (String.concat "; " (List.map string_of_float (Array.to_list arr2)))
      result expected;
    assert (result = expected)
  ) test_cases
