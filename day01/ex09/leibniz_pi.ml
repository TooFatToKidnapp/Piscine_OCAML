let leibniz_pi (delta : float) : int =
  if delta < 0. then -1
  else
    let ref_pi = 4.0 *. atan 1.0 in
    let rec loop sigma n =
      let approx_pi = 4.0 *. sigma in
      let difference = approx_pi -. ref_pi in
      if (if difference < 0. then -.difference else difference) <= delta then n
      else
        let i = float_of_int n in
        loop (sigma +. ((-1.0) ** i /. ((2.0 *. i) +. 1.0))) (n + 1)
    in
    loop 0.0 0


let () =
  print_int (leibniz_pi 0.1);
  print_char '\n'
