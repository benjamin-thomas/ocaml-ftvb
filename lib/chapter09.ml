(* Partially apply a function *)
let add6 = ( + ) 6

let%test_unit _ = [%test_eq: Base.int] 7 @@ add6 1

let%test_unit _ =
  [%test_eq: Base.int Base.list] [ 7; 8; 9 ] @@ List.map add6 [ 1; 2; 3 ]

(* The next two function calls are equivalent *)
let%test_unit _ =
  [%test_eq: Base.int Base.list] [ 2; 4; 6 ]
  @@ List.map (fun n -> n * 2) [ 1; 2; 3 ]

let%test_unit _ =
  [%test_eq: Base.int Base.list] [ 2; 4; 6 ] @@ List.map (( * ) 2) [ 1; 2; 3 ]

(* Originally from chapter 6 *)
let ( == ) =
  let open Base in
  [%test_eq: int list]

let rec map fn lst =
  match lst with
  | [] -> []
  | h :: t -> fn h :: map fn t

let halve x = x / 2

let%test_unit _ = map halve [ 2; 4; 6 ] == [ 1; 2; 3 ]

(* 8. Write a function `mapl` which maps a function of type `'a -> 'b` over a list of type `'a list list`
      to produce a list of type `'b list list` *)

let rec mapl f lst =
  match lst with
  | [] -> []
  | x :: xs -> map f x :: mapl f xs

let double x = x * 2

let%test _ = mapl double [ [ 1; 2 ]; [ 2; 3 ] ] = [ [ 2; 4 ]; [ 4; 6 ] ]

(* `mapl` can be written with partial application *)
let mapl' fn lst = map (map fn) lst

let%test _ = mapl double [ [ 1; 2 ]; [ 2; 3 ] ] = [ [ 2; 4 ]; [ 4; 6 ] ]

(* The next two functions are equivalent. *)
let add x y = x + y
let add' = fun x -> fun y -> x + y [@@ocamlformat "disable"]

(* Questions *)

(* 1. Rewrite the summary paragraph at the end of the chapter for the three argument function `g a b c` *)

let g = fun a -> fun b -> fun c -> a + b + c [@@ocamlformat "disable"]

(* 2.
   Remember the function `member x l` which determines if an element `x` is
   contained in a list `l`.contents

   What is the type of `member x` ? => 'a -> 'a list -> bool

   Use partial application to write a function `member_all x ls` which determines
   if an element is a member of all the lists in the list of lists `ls`
*)

(* From chapter 4 *)

let rec member elem lst =
  match lst with
  | [] -> false
  | x :: xs -> x = elem || member elem xs

let%test _ = member '?' [] = false
let%test _ = member 2 [ 1; 2; 3 ] = true
let%test _ = member 'o' [ 'h'; 'e'; 'l'; 'l'; 'o' ] = true
let%test _ = member 'z' [ 'h'; 'e'; 'l'; 'l'; 'o' ] = false

(* My attempt *)
let member_all x lss = List.for_all (member x) lss

(* From the book. This one is insane! *)
let member_all' x lss =
  let bools = map (member x) lss in
  not (member false bools)

let%test _ = member_all 2 [ [ 1; 2; 3 ]; [ 2; 3; 4 ] ] = true
let%test _ = member_all' 2 [ [ 1; 2; 3 ]; [ 2; 3; 4 ] ] = true

(* 3.

   Why can we not write a function to halve all the elements of a list like this?
     map (( / ) 2) [10;20;30]

   Because this applies 2/10, 2/20, 2/30 instead of 10/2, 20/2, 30/2

   Write a suitable division function which can be partially applied in the manner we require.
*)

let div_by y x = x / y

let%test _ = map (div_by 2) [ 10; 20; 30 ] = [ 5; 10; 15 ]

(* 4.
   Write a function `mapll` which maps a function over a lists of lists of lists.
   You must not use the `let rec` construct.
*)

let mapll fn lsss = map (map (map fn)) lsss

let%test _ =
  mapll (div_by 2) [ [ [ 10; 20 ]; [ 30; 40 ] ]; [ [ 30; 40 ]; [ 50; 60 ] ] ]
  = [ [ [ 5; 10 ]; [ 15; 20 ] ]; [ [ 15; 20 ]; [ 25; 30 ] ] ]

(* 5.
   Write a function `truncate` which takes an integer and a list of lists, and returns a list of lists, each
   of which has been truncated to the given length.

   If a list is shorter than the given length, it is unchanged. Make use of partial functions.
*)

let truncate_one n lst =
  let rec inner n lst =
    if n > 0 then
      inner (n - 1) (List.tl lst)
    else
      lst
  in
  if List.length lst < n then
    lst
  else
    inner n lst

let%test_unit _ = [%test_eq: Base.int Base.list] [] @@ truncate_one 1 []

let%test_unit _ =
  [%test_eq: Base.int Base.list] [ 2; 3 ] @@ truncate_one 1 [ 1; 2; 3 ]

let%test_unit _ =
  [%test_eq: Base.int Base.list] [] @@ truncate_one 3 [ 1; 2; 3 ]

let%test_unit "list is shorter than given length" =
  [%test_eq: Base.int Base.list] [ 1; 2; 3 ] @@ truncate_one 4 [ 1; 2; 3 ]

let truncate_all n lss = map (truncate_one n) lss

let%test _ =
  truncate_all 1 [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8 ] ]
  = [ [ 2; 3 ]; [ 5; 6 ]; [ 8 ] ]

let%test _ =
  truncate_all 3 [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8 ] ] = [ []; []; [ 7; 8 ] ]

(* 6.

    Write a function which takes a list of lists of integers and returns the list composed of all the
    first elements of the lists.

    If a list is empty, a given number should be used in place of its first element.
*)

let hd_or n lst =
  match lst with
  | [] -> n
  | x :: _ -> x

let first_or default lss = map (hd_or default) lss

let%test _ = first_or 99 [ [ 1; 2; 3 ]; [ 4 ]; []; [ 7; 8 ] ] = [ 1; 4; 99; 7 ]
