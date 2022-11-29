(* Same exercises with non empty lists *)

type 'a non_empty_list = 'a * 'a list

(*
   CONS
*)
let cons item (x, xs) = (item, x :: xs)

let%test _ = cons 1 (2, [ 3; 4 ]) = (1, [ 2; 3; 4 ])

(*
    NIL: not representable
*)

(*
    LENGTH
*)
let rec len (nel : 'a non_empty_list) =
  match nel with
  | _, [] -> 1
  | _, x :: xs -> 1 + len (x, xs)

let%test _ = len ('a', []) = 1
let%test _ = len (1, [ 2; 3 ]) = 3

let len_tr (nel : 'a non_empty_list) =
  let rec inner nel acc =
    match nel with
    | _, [] -> acc
    | _, x :: xs -> inner (x, xs) (acc + 1)
  in
  inner nel 1

let%test _ = len_tr ('a', []) = 1
let%test _ = len_tr (1, [ 2; 3 ]) = 3

(*
   SUM
*)
let sum (nel : 'a non_empty_list) =
  let rec inner nel acc =
    match nel with
    | w, [] -> acc + w
    | w, x :: xs -> inner (x, xs) (acc + w)
  in
  inner nel 0

let%test _ = sum (9, []) = 9
let%test _ = sum (9, [ 8 ]) = 17
let%test _ = sum (9, [ 8; 7 ]) = 24

(*
  ODD_ELEMENTS
*)

let rec odd_elements (nel : 'a non_empty_list) =
  match nel with
  | w, [] -> (w, [])
  | w, _ :: [] -> (w, [])
  | w, _ :: x :: xs -> cons w @@ odd_elements (x, xs)

let%test _ = odd_elements (1, []) = (1, [])
let%test _ = odd_elements (1, [ 2 ]) = (1, [])
let%test _ = odd_elements (1, [ 2; 3 ]) = (1, [ 3 ])
let%test _ = odd_elements (1, [ 2; 3; 4 ]) = (1, [ 3 ])
let%test _ = odd_elements (1, [ 2; 3; 4; 5 ]) = (1, [ 3; 5 ])

(*
  EVEN_ELEMENTS
*)
let even_elements (nel : 'a non_empty_list) =
  (* I had difficulties dealing with the original data structure, so choose to temporarily deal with a list instead *)
  let rec inner lst =
    match lst with
    | [] -> []
    | _ :: [] -> []
    | _ :: x :: xs -> x :: inner xs
  in
  match inner (fst nel :: snd nel) with
  | [] ->
      failwith "Cannot select even elements on a NonEmptyList with one element!"
  | x :: xs -> (x, xs)

(* let%test _ = even_elements (1, []) = (-1, []) *)
let%test _ = even_elements (1, [ 2 ]) = (2, [])
let%test _ = even_elements (1, [ 2; 3 ]) = (2, [])
let%test _ = even_elements (1, [ 2; 3; 4 ]) = (2, [ 4 ])
let%test _ = even_elements (1, [ 2; 3; 4; 5 ]) = (2, [ 4 ])
(*
    APPEND
*)

let append (nel_a : 'a non_empty_list) (nel_b : 'a non_empty_list) =
  (* Same as with `even_elements`. I find using the original data structure difficult! *)
  let rec inner a b =
    match a with
    | [] -> b
    | x :: xs -> x :: inner xs b
  in
  let lst = inner (fst nel_a :: snd nel_a) (fst nel_b :: snd nel_b) in
  match lst with
  | [] -> failwith "This code is impossible to reach, whatever the input!"
  | x :: xs -> (x, xs)

let%test _ = append (1, []) (2, []) = (1, [ 2 ])
let%test _ = append (1, []) (2, [ 3 ]) = (1, [ 2; 3 ])
let%test _ = append (1, [ 2 ]) (3, []) = (1, [ 2; 3 ])

(*
  TODO: rev, take, drop, count_true, to_palindrome, is_palindrome, drop_last, member, make_set
*)