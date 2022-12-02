(* Test helpers below *)
let ( == ) =
  let open Base in
  [%test_eq: int list]

let ( =? ) =
  let open Base in
  [%test_eq: bool list]

let ( === ) =
  let open Base in
  [%test_eq: int]

(* Program starts here *)

let rec double lst =
  match lst with
  | [] -> []
  | h :: t -> (h * 2) :: double t

let%test_unit _ = double [] == []
let%test_unit _ = double [ 1 ] == [ 2 ]
let%test_unit _ = double [ 1; 2 ] == [ 2; 4 ]

let rec evens lst =
  match lst with
  | [] -> []
  | h :: t -> (h mod 2 = 0) :: evens t

let%test_unit _ = evens [] =? []
let%test_unit _ = evens [ 1 ] =? [ false ]
let%test_unit _ = evens [ 1; 2 ] =? [ false; true ]

let rec map f lst =
  match lst with
  | [] -> []
  | h :: t -> f h :: map f t

let halve x = x / 2

let%test_unit _ = map halve [ 2; 4; 6 ] == [ 1; 2; 3 ]

let is_even x = x mod 2 = 0
let evens' lst = map is_even lst
let evens'' = map is_even
let evens''' lst = map (fun x -> x mod 2 = 0) lst
let evens'''' = map (fun x -> x mod 2 = 0)

let%test_unit _ = evens' [] =? []
let%test_unit _ = evens' [ 1 ] =? [ false ]
let%test_unit _ = evens' [ 1; 2 ] =? [ false; true ]
let%test_unit _ = evens'' [ 1; 2 ] =? [ false; true ]
let%test_unit _ = evens''' [ 1; 2 ] =? [ false; true ]
let%test_unit _ = evens'''' [ 1; 2 ] =? [ false; true ]

let length lst =
  let rec inner lst n =
    match lst with
    | [] -> n
    | _ :: xs -> inner xs (n + 1)
  in
  inner lst 0

let rec take n lst =
  let _ = assert (n >= 0) in
  match (n, lst) with
  | _, [] -> []
  | 0, _ -> []
  | _, x :: xs -> x :: take (n - 1) xs

let rec drop n lst =
  let _ = assert (n >= 0) in
  match (n, lst) with
  | _, [] -> lst
  | 0, _ -> lst
  | _, _ :: xs -> drop (n - 1) xs

let greater a b = a >= b

let rec merge cmp x y =
  match (x, y) with
  | [], lst -> lst
  | lst, [] -> lst
  | hx :: tx, hy :: ty ->
      if cmp hx hy then
        hx :: merge cmp tx (hy :: ty)
      else
        hy :: merge cmp (hx :: tx) ty

let rec msort cmp lst =
  let half = length lst / 2 in
  match lst with
  | [] -> []
  | [ x ] -> [ x ]
  | _ ->
      let left = take half lst in
      let right = drop half lst in
      merge cmp (msort cmp left) (msort cmp right)

let%test_unit _ = msort greater [ 1; 3; 2; 5; 4 ] == [ 5; 4; 3; 2; 1 ]
let%test_unit _ = msort ( >= ) [ 1; 3; 2; 5; 4 ] == [ 5; 4; 3; 2; 1 ]
let%test_unit _ = msort ( <= ) [ 1; 3; 2; 5; 4 ] == [ 1; 2; 3; 4; 5 ]

(*
  EXERCISES
*)

(* 1. Write a simple recursive function `calm` to replace exclamation marks in a `char list` with periods.
      The re-write it with `map` instead of recursion. *)

let rec calm chars =
  match chars with
  | [] -> []
  | x :: xs ->
      if x = '!' then
        '.' :: calm xs
      else
        x :: calm xs

let%test _ =
  calm [ 'H'; 'e'; 'l'; 'l'; 'o'; '!' ] = [ 'H'; 'e'; 'l'; 'l'; 'o'; '.' ]

let calm' chars =
  chars
  |> map @@ fun c ->
     if c = '!' then
       '.'
     else
       c

let%test _ =
  calm' [ 'H'; 'e'; 'l'; 'l'; 'o'; '!' ] = [ 'H'; 'e'; 'l'; 'l'; 'o'; '.' ]

(* 2.

   Write a function `clip` which, given an integer, clips it to the range `1 ... 10` so that
   integers bigger than 10 round down to 10, and thos smaller than 1 round up to 1.contents

   Write another function `cliplist` which uses the first function together with `map` to
   apply this clipping to a whole list of integers.
*)

let clip n =
  if n > 10 then
    10
  else
    n

let cliplist = map clip

let%test _ = cliplist [ 1; 5; 10; 99 ] = [ 1; 5; 10; 10 ]

(* 3. Express your function `cliplist` again, this time using an anonymous function isntead of `clip` *)

let cliplist' =
  map (fun n ->
      if n > 10 then
        10
      else
        n)

let%test _ = cliplist' [ 1; 5; 10; 99 ] = [ 1; 5; 10; 10 ]

(* 4. Write a function `apply` which, given another function, a number of times to apply it, and  an initial
      argument for the function, will return the cumulative effect of repeatedly applying the function.

      For instance, `apply f 6 4` should return `f (f (f (f (f (f 4))))))`*)

let rec apply f x n =
  match n with
  | 0 -> failwith "invalid arg"
  | 1 -> f x
  | _ -> f (apply f x (n - 1))

let f n = n * 2

let%test_unit _ = f 2 === 4
let%test_unit _ = f (f 2) === 8
let%test_unit _ = f (f (f 2)) === 16
let%test_unit _ = apply f 2 1 === 4
let%test_unit _ = apply f 2 2 === 8
let%test_unit _ = apply f 2 3 === 16

(* 5. Modify the insertion sort function from the preceding chapter to take a comparison function, en the same way we modified merge sort in this chapter *)

let rec inssort cmp lst =
  let rec insert a lst =
    match lst with
    | [] -> [ a ]
    | x :: xs ->
        if cmp a x then
          a :: insert x xs
        else
          x :: insert a xs
  in
  match lst with
  | [] -> []
  | x :: xs -> insert x (inssort cmp xs)

let%test_unit _ = inssort ( <= ) [ 1; 3; 2; 5; 4 ] == [ 1; 2; 3; 4; 5 ]
let%test_unit _ = inssort ( >= ) [ 1; 3; 2; 5; 4 ] == [ 5; 4; 3; 2; 1 ]

(* 6. Write a `filter` which takes a function of type `'a -> bool` and an `'a list` and returns a list
      of just those elements of the argument list for which the given function returns true *)

let rec filter f lst =
  match lst with
  | [] -> []
  | x :: xs ->
      if f x then
        x :: filter f xs
      else
        filter f xs

let identity x = x
let ( << ) f g x = x |> g |> f
let eq = ( = )
let lte b a = a <= b

[@@@ocamlformat "disable"]

let%test _ = filter (lte 3)           [ 1; 2; 3; 4 ]        = [ 1; 2; 3 ]
let%test _ = filter (not << lte 3)    [ 1; 2; 3; 4 ]        = [ 4 ]
let%test _ = filter (fun x -> x <= 3) [ 1; 2; 3; 4 ]        = [ 1; 2; 3 ]
let%test _ = filter identity          [ true; false; true ] = [ true; true ]
let%test _ = filter (not << identity) [ true; false; true ] = [ false ]
let%test _ = filter (eq 1)            [ 1; 1; 2; 2 ]        = [ 1; 1 ]
let%test _ = filter (not << eq 1)     [ 1; 1; 2; 2 ]        = [ 2; 2 ]

[@@@ocamlformat "enable"]

(* 7. Write the function `for_all` which, given a function of type `'a -> bool` and an argument list of type `'a list`
   evaluates to true if and only if the function returns true for every element of the list *)

let rec for_all f lst =
  match lst with
  | [] -> true
  | x :: xs ->
      if f x then
        for_all f xs
      else
        false

[@@@ocamlformat "disable"]

let%test _ =        for_all identity [ true; true; true ]
let%test _ = not @@ for_all identity [ true; false; true ]
let%test _ =        for_all (lte 3)  [ 1; 2; 3 ]
let%test _ = not @@ for_all (lte 3)  [ 1; 2; 4 ]

[@@@ocamlformat "enable"]

(* 8. Write a function `mapl` which maps a function of type `'a -> 'b` over a list of type `'a list list`
      to produce a list of type `'b list list` *)

let rec mapl f lst =
  match lst with
  | [] -> []
  | x :: xs -> map f x :: mapl f xs

let double x = x * 2

let%test _ = mapl double [ [ 1; 2 ]; [ 2; 3 ] ] = [ [ 2; 4 ]; [ 4; 6 ] ]
