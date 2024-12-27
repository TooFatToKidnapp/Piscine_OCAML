let () =
	let print t =
		match t with
		| Try.Try.Failure (_) -> print_endline "Failure"
		| Try.Try.Success (_) -> print_endline "Success"
	in
	let null = Try.Try.return 1 in
	let b = Try.Try.bind null (fun x -> Try.Try.return (1 / x)) in
	print null;
	print b;
	let test = Try.Try.return "String_test" in
	let fn (s:string): bool = String.length s > 20 in
	let result = Try.Try.filter test fn in
	print result;
	let fla = Try.Try.return (result) in
	print (Try.Try.flatten fla);
	let fn_error (e:exn) = match e with
		| _ -> Try.Try.return "I am recovered !"
	in
	let recovered = Try.Try.recover result fn_error in
	print recovered;
	let fla2 = Try.Try.return (Try.Try.return "Inception ...") in
	print (Try.Try.flatten fla2);
