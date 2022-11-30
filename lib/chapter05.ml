(* Test helpers below *)
let ( == ) =
  let open Base in
  [%test_eq: int list]

(* Program starts here *)

let rec insert a lst =
  match lst with
  | [] -> [ a ]
  | x :: xs -> if   a <= x
               then a :: x :: xs
               else x :: insert a xs
               [@@ocamlformat "disable"]

let%test _ = insert 1 [ 1; 2 ] = [ 1; 1; 2 ]
let%test _ = insert 2 [ 1; 2 ] = [ 1; 2; 2 ]
let%test _ = insert 3 [ 1; 2 ] = [ 1; 2; 3 ]

let rec sort lst =
  match lst with
  | [] -> []
  | x :: xs -> insert x (sort xs)

let%test _ = sort [ 1; 2; 3 ] = [ 1; 2; 3 ]
let%test _ = sort [ 3; 2; 1 ] = [ 1; 2; 3 ]
let%test _ = sort [ 1; 2; 3 ] = [ 1; 2; 3 ]

(*
Cost is O(n²)
00 sort [ 3; 2; 1 ]
01   ~> insert 3 (sort [ 2; 1 ])
02   ~> insert 3 (insert 2 (sort [ 1 ]))
03   ~> insert 3 (insert 2 (insert 1 []))
04    ~+> insert 3 (insert 2 [1])
05    ~+> insert 3 (1 :: insert 2 [])
06    ~+> insert 3 [ 1; 2 ]
07    ~+> 1 :: insert 3 [ 2 ]
08    ~+> 1 :: 2 :: insert 3 []
09    ~+> 1 :: 2 :: [3]
*)

let rec merge x y =
  match (x, y) with
  | [], lst -> lst
  | lst, [] -> lst
  | hx :: tx, hy :: ty ->
      if   hx < hy
      then hx :: merge tx (hy :: ty)
      else hy :: merge (hx :: tx) ty
      [@@ocamlformat "disable"]

let%test _ = merge [] [] = []
let%test _ = merge [] [ 1 ] = [ 1 ]
let%test _ = merge [ 1 ] [] = [ 1 ]
let%test _ = merge [ 9; 53 ] [ 2; 6; 19 ] = [ 2; 6; 9; 19; 53 ]

let rec msort lst =
  let open Chapter04 in
  match lst with
  | [] -> []
  | [ x ] -> [ x ]
  | _ ->
      let half = length_tr lst / 2 in
      let left = take half lst in
      let right = drop half lst in
      merge (msort left) (msort right)

let%test _ = msort [ 3; 2; 1 ] = [ 1; 2; 3 ]
let%test _ = msort [ 2; 3; 1 ] = [ 1; 2; 3 ]
let%test _ = msort [ 1; 2; 3 ] = [ 1; 2; 3 ]

(*
   EXERCISES

   Not requiring a new function:
    1.
    2.
*)

(* 3. Write a version of insertion sort that orders the items in revers order. *)

let rec ins_rev h1 t1 =
  match t1 with
  | [] -> [ h1 ]
  | h2 :: t2 ->
      match h1 > h2 with
      | true  -> ins_rev h1 t2 @ [ h2 ]
      | false -> ins_rev h2 t2 @ [ h1 ]
  [@@ocamlformat "disable"]

let%test _ = ins_rev 3 [] = [ 3 ]
let%test _ = ins_rev 3 [ 1 ] = [ 3; 1 ]
let%test_unit _ = ins_rev 3 [ 1; 2 ] == [ 3; 2; 1 ]

let rec rev_inssort lst =
  match lst with
  | [] -> []
  | h :: t -> ins_rev h (rev_inssort t)

let%test _ = ins_rev 3 [] = [ 3 ]
let%test _ = ins_rev 3 [ 1 ] = [ 3; 1 ]
let%test _ = ins_rev 3 [ 1; 2 ] = [ 3; 2; 1 ]
let%test_unit _ = rev_inssort [] == []
let%test_unit _ = rev_inssort [ 1 ] == [ 1 ]
let%test_unit _ = rev_inssort [ 1; 2 ] == [ 2; 1 ]
let%test_unit _ = rev_inssort [ 1; 2; 3 ] == [ 3; 2; 1 ]

(* 4. Write a function that detects if a list is already sorted *)
let rec is_sorted lst =
  match lst with
  | [] -> true
  | [ _ ] -> true
  | a :: b :: t -> if a > b then false else is_sorted t

let%test _ = is_sorted []
let%test _ = is_sorted [ 1 ]
let%test _ = is_sorted [ 1; 1 ]
let%test _ = is_sorted [ 1; 2 ]
let%test _ = is_sorted [ 2; 1 ] |> not
(* 5. Play with list comparaisons *)
let%test _ = not ([] < [])
let%test _ = not ([] > [])
let%test _ = [] <= []
let%test _ = [] >= []
let%test _ = [ 'a' ] < [ 'b' ]
let%test _ = [ 'b' ] > [ 'a' ]
let%test _ = [ 1 ] < [ 2 ]
let%test _ = [ 1; 2 ] < [ 2 ]
(* compare must only look at the first item *)
let%test _ = [ 1; 9 ] < [ 2; 30; 40 ]

(* 6. Combine the sort and insert functions into a single sort function *)

let rec sort' lst =
  let rec insert' a lst =
    match lst with
    | [] -> [ a ]
    | x :: xs -> (
        match a <= x with
        | true -> a :: x :: xs
        | false -> x :: insert' a xs)
  in
  match lst with
  | [] -> []
  | x :: xs -> insert' x (sort' xs)

let%test _ = sort' [ 1; 2; 3 ] = [ 1; 2; 3 ]
let%test _ = sort' [ 3; 2; 1 ] = [ 1; 2; 3 ]
let%test _ = sort' [ 1; 2; 3 ] = [ 1; 2; 3 ]