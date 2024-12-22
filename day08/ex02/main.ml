module Calc_int = Calc.Calc((Calc.INT))
module Calc_float = Calc.Calc(Calc.FLOAT)

let () =
  let int_power = Calc_int.power 3 3 in
  let int_mul_add = Calc_int.mul (Calc_int.add 20 1) 2 in
  let int_fact = Calc_int.fact 5 in
  Printf.printf "Integer power (3^3): %d\n" int_power;
  Printf.printf "Integer mul_add ((20 + 1) * 2): %d\n" int_mul_add;
  Printf.printf "Integer factorial (5!): %d\n" int_fact;


  let float_power = Calc_float.power 3.0 3 in
  let float_mul_add = Calc_float.mul (Calc_float.add 20.0 1.0) 2.0 in
  let float_fact = Calc_float.fact 5.0 in
  Printf.printf "Float power (3.0^3): %f\n" float_power;
  Printf.printf "Float mul_add ((20.0 + 1.0) * 2.0): %f\n" float_mul_add;
  Printf.printf "Float factorial (5.0!): %f\n" float_fact
