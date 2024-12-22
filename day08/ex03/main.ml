
let () =
    let string_of_int_try t = match t with
        | Try.Try.Success suc -> "Success {" ^ string_of_int suc ^ "}"
        | Try.Try.Failure (Invalid_argument mess) -> mess
        | _ -> "Unknown failure" in

    let div a b = if b = 0
        then Try.Try.Failure (Invalid_argument "are you trying to divide by zero?")
        else Try.Try.return (a / b) in

    print_endline "\n===testing Success and return===" ;
    let try_ok = div 42 2 in print_endline (string_of_int_try try_ok) ;

    print_endline "\n===testing failure===" ;
    let try_ko = div 42 0 in print_endline (string_of_int_try try_ko) ;

    print_endline "\n===testing bind===" ;
    print_endline (string_of_int_try (Try.Try.bind try_ok (div 142))) ;
    print_endline (string_of_int_try (Try.Try.bind try_ko (div 142))) ;
    let to_string a = if a = 0 then Try.Failure (Invalid_argument "je n'imprimerai pas 0") else Try.return (string_of_int a) in
    let string_of_str_try t = match t with
        | Try.Try.Success suc -> "Success {" ^ suc ^ "}"
        | Try.Try.Failure (Invalid_argument mess) -> mess
        | _ -> "Unknown failure" in
    print_endline (string_of_str_try (Try.Try.bind (Try.Try.return 0) to_string)) ;
    print_endline (string_of_str_try (Try.Try.bind (Try.Try.return 42) to_string)) ;

    print_endline "\n===testing recover===" ;
    let set_zero ex = Try.Try.Success 0 in
    print_endline (string_of_int_try (Try.Try.recover try_ko set_zero)) ;
    let is_divisible_by d i = i mod d = 0 in
    print_endline (string_of_int_try (Try.Try.recover try_ko set_zero)) ;

    print_endline "\n===testing filter===" ;
    print_endline (string_of_int_try (Try.Try.filter try_ok (is_divisible_by 2))) ;
    print_endline (string_of_int_try (Try.Try.filter try_ok (is_divisible_by 3))) ;

    print_endline "\n===testing flatten===" ;
    let meta_ok = Try.Try.Success try_ok in print_endline (string_of_int_try (Try.Try.flatten meta_ok)) ;
    let meta_ko = Try.Try.Success try_ko in print_endline (string_of_int_try (Try.Try.flatten meta_ko)) ;
