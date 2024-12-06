(* https://www.cs.cmu.edu/Groups/AI/html/faqs/ai/genetic/part6/faq-doc-1.html *)
let gray c =
  let rec aux n =
    match n with
    | 0 -> ["0"]
    | 1 -> ["0"; "1"]
    | _ ->
        let previous = aux (n - 1) in
        let add_prefix prefix lst =
          let rec map = function
          | [] -> []
          | h::t -> (prefix ^ h) :: map t
        in
          map lst
        in
          let rec rev_list acc = function
            | [] -> acc
            | h::t -> rev_list (h :: acc) t
          in
          let rec concat_list lst1 lst2 =
          match lst1 with
            | [] -> lst2
            | h::t -> h :: concat_list t lst2
            in
          concat_list (add_prefix "0" previous) (rev_list [] (add_prefix "1" previous))
      in
      let print_gray_sequence n =
      let sequence = aux n in
      let rec iter fn = function
        | [] -> ()
        | h::t -> fn h; iter fn t;
      in
      iter (fun s -> print_string s; print_char ' ') sequence;
      print_newline ()
      in print_gray_sequence c

let () =
gray 1;
gray 2;
gray 3


(*
  0 1
  00 01 11 10
  000 001 011 010 110 111 101 100
*)
