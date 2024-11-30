let sum f1 f2 =
  f1 +. f2

let () =
  print_float (sum (-.6.2) 11.5) ;
  print_char '\n';
  print_float (sum 0.0 4.69) ;
  print_char '\n' ;
  print_float (sum 69. 4.5) ;
  print_char '\n' ;
