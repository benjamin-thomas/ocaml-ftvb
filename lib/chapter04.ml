(* Equality *)
let%test _ = [ 1; 2; 3 ] = [ 1; 2; 3 ]
let%test _ = [ 1; 2; 3 ] <> [ 1; 3; 2 ]

(* Cons operator `::` prepends an item to a list, in **contstant** time O(1) *)
let%test _ =
  1 :: [ 2; 3 ] = [ 1; 2; 3 ]
  [@@ocamlformat "disable"]

(* Add operator `@` appends a list to another list, **linear** time O(n) *)
let%test _ =
  [ 1; 2 ] @ [ 3 ] = [ 1; 2; 3 ]
  [@@ocamlformat "disable"]

(* Use pattern matching to detect an empty list (the list is "nil" in Ocaml parlance) *)
let isnil lst =
  match lst with
  | [] -> true
  | _ -> false

let%test _ = isnil [] = true
let%test _ = isnil [ 1 ] = false

(* `length` here accepts a generic list of "alpha" ('a list). This function is said to be "polymorphic" *)
let rec length lst =
  match lst with
  | [] -> 0
  | _ :: xs -> 1 + length xs

let%test _ = length [] = 0
let%test _ = length [ 2; 4; 6 ] = 3
let%test _ = length [ 'A'; 'B'; 'C' ] = 3
(*
length [ 2; 4; 6 ]
    ~> 1 + length [4; 6]
    ~> 1 + 1 + length [6]
    ~> 1 + 1 + 1 + length []
    ~> 1 + 1 + 1 + 0
    ~> 1 + 1 + 1
    ~> 1 + 2
    ~> 3
*)

(* Since `sum` uses the head of the list, it is not polymorphic (it operates on `int list` only) *)
let rec sum lst =
  match lst with
  | [] -> 0
  | x :: xs -> x + sum xs

let%test _ = sum [] = 0
let%test _ = sum [ 1; 1 ] = 2
let%test _ = sum [ 1; 3 ] = 4

(*
   Avoid large build ups of 1 + 1 + 1 ... by passing an accumlator internally.
   Recursive functions which do not build up a growing intermediate representation are known as "tail recursive".
 *)
let length_tr lst =
  let rec inner lst n =
    match lst with
    | [] -> n
    | _ :: xs -> inner xs (n + 1)
  in
  inner lst 0

let%test _ = length_tr [] = 0
let%test _ = length_tr [ 2; 4; 6 ] = 3
(*
length2    [ 2; 4; 6 ]
 ~> inner  [ 2; 4; 6 ] 0
 ~> inner  [ 4; 6 ] 1
 ~> inner  [ 6 ] 2
 ~> inner  [] 3
 ~> inner  3
*)

let sum2 lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | x :: xs -> inner xs (acc + x)
  in
  inner lst 0

let%test _ = sum2 [] = 0
let%test _ = sum2 [ 1; 1 ] = 2
let%test _ = sum2 [ 1; 3 ] = 4

let rec odd_elements lst =
  match lst with
  | x :: _ :: xs -> x :: odd_elements xs
  | _ -> lst

let%test _ = odd_elements [] = []
let%test _ = odd_elements [ 1 ] = [ 1 ]
let%test _ = odd_elements [ 1; 2 ] = [ 1 ]
let%test _ = odd_elements [ 1; 2; 4 ] = [ 1; 4 ]

let rec append a b =
  let lst =
    match a with
    | [] -> b
    | x :: xs -> x :: append xs b
  in
  lst

let%test _ = append [] [] = []
let%test _ = append [ 1 ] [] = [ 1 ]
let%test _ = append [ 1; 2 ] [ 3 ] = [ 1; 2; 3 ]
(*
append [ 1; 2 ] [ 3 ]
    ~> 1 :: append [ 2 ] [ 3 ]
    ~> 1 :: 2 :: append [] [ 3 ]
    ~> 1 :: 2 :: [ 3 ]
    ~> [1; 2; 3]
*)

let rec rev lst =
  match lst with
  | [] -> []
  | x :: xs -> rev xs @ [ x ]

let%test _ = rev [ 1; 2; 3 ] = [ 3; 2; 1 ]
(*
rev [ 1; 2; 3 ]
 ~> [ 2; 3 ] @ [1]
 ~> [ 3 ] @ [2] @ [1]
 ~> [] @ [3] @ [2] @ [1]
 ~> [3; 2, 1]
*)

let rec take n lst =
  let _ = assert (n >= 0) in
  match (n, lst) with
  | _, [] -> []
  | 0, _ -> []
  | _, x :: xs -> x :: take (n - 1) xs

let%test _ = take 0 [ 1; 2; 3 ] = []
let%test _ = take 1 [ 1; 2; 3 ] = [ 1 ]
let%test _ = take 2 [ 1; 2; 3 ] = [ 1; 2 ]
let%test _ = take 3 [ 1; 2; 3 ] = [ 1; 2; 3 ]
let%test _ = take 4 [ 1; 2; 3 ] = [ 1; 2; 3 ]

let rec drop n lst =
  match (n, lst) with
  | _, [] -> lst
  | 0, _ -> lst
  | _, _ :: xs -> drop (n - 1) xs

let%test _ = drop 0 [ 1; 2; 3 ] = [ 1; 2; 3 ]
let%test _ = drop 1 [ 1; 2; 3 ] = [ 2; 3 ]
let%test _ = drop 2 [ 1; 2; 3 ] = [ 3 ]
let%test _ = drop 3 [ 1; 2; 3 ] = []
let%test _ = drop 4 [ 1; 2; 3 ] = []

(*
    EXERCISES
*)

(* 1. Write a function `evens` which returns every even element of a list *)
let rec evens lst =
  match lst with
  | [] -> []
  | _ :: [] -> []
  | _ :: h2 :: t -> h2 :: evens t

let%test _ = evens [] = []
let%test _ = evens [ 1 ] = []
let%test _ = evens [ 1; 3 ] = [ 3 ]
let%test _ = evens [ 1; 3; 9 ] = [ 3 ]
let%test _ = evens [ 1; 3; 9; 12 ] = [ 3; 12 ]

(* 2. Write a function `count_true` which returns the number of true elements in a list *)

let count_true lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | x :: xs -> inner xs (acc + if x = true then 1 else 0)
  in
  inner lst 0

let%test _ = count_true [] = 0
let%test _ = count_true [ false ] = 0
let%test _ = count_true [ true ] = 1
let%test _ = count_true [ true; false ] = 1
let%test _ = count_true [ true; false; false ] = 1
let%test _ = count_true [ true; false; true ] = 2

(* 3a. Write a function which, given a list, builds a palindrome of it (a list equal to its own reverse).
       Ok to use `rev` and `@`.

   I didn't quite understand what I was supposed to do here... *)
let to_palindrome lst =
  match List.rev lst = lst with
  | true -> lst
  | false -> []

let%test _ = to_palindrome [] = []
let%test _ = to_palindrome [ 'd'; 'a'; 'y' ] = []
let%test _ = to_palindrome [ 'n'; 'o'; 'o'; 'n' ] = [ 'n'; 'o'; 'o'; 'n' ]

(* 3b. Write another function which determines if a list is a palindrome. *)
let is_palindrome lst = List.rev lst = lst

let%test _ = is_palindrome [ 'k'; 'a'; 'y'; 'a'; 'k' ] = true
let%test _ = is_palindrome [ 'h'; 'e'; 'l'; 'l'; 'o' ] = false

(* 4. Write a function `drop_last` which returns all but the last element of a list.
      If a list is empty, it should return an empty list.

      Example: [1;2;4;8] => [1;2;4]

      Then make it tail recursive.
*)

let rec drop_last lst =
  match lst with
  | [] -> []
  | _ :: [] -> []
  | x :: xs -> x :: drop_last xs

let%test _ = drop_last [] = []
let%test _ = drop_last [ 1 ] = []
let%test _ = drop_last [ 1; 2 ] = [ 1 ]
let%test _ = drop_last [ 1; 2; 3 ] = [ 1; 2 ]

let drop_last_tr lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | _ :: [] -> acc
    | x :: xs -> inner xs (x :: acc)
  in
  (* Reversing at the end since `@` is not tail recursive *)
  inner lst [] |> List.rev

let%test _ = drop_last_tr [] = []
let%test _ = drop_last_tr [ 1 ] = []
let%test _ = drop_last_tr [ 1; 2 ] = [ 1 ]
let%test _ = drop_last_tr [ 1; 2; 3 ] = [ 1; 2 ]

(* 5. Write a function `member` of type ['a -> 'a list-> bool] which returns
      true if an element exists in a list, otherwise false. *)

let rec member elem lst =
  match lst with
  | [] -> false
  | x :: xs -> x = elem || member elem xs

let%test _ = member '?' [] = false
let%test _ = member 2 [ 1; 2; 3 ] = true
let%test _ = member 'o' [ 'h'; 'e'; 'l'; 'l'; 'o' ] = true
let%test _ = member 'z' [ 'h'; 'e'; 'l'; 'l'; 'o' ] = false

(* 6. Use your `member` function to write a function `make_set` which, given a list, returns a list
      which contains all the elements of the original list, but has no duplicate elements. *)

let make_set lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | x :: xs -> inner xs (if member x acc then acc else x :: acc)
  in
  inner lst [] |> List.rev

let%test _ = make_set [] = []
let%test _ = make_set [ 1 ] = [ 1 ]
let%test _ = make_set [ 1; 1 ] = [ 1 ]
let%test _ = make_set [ 1; 1; 3; 3; 2; 2 ] = [ 1; 3; 2 ]
let%test _ = make_set [ 1; 2; 3; 3; 1 ] = [ 1; 2; 3 ]

(* 7. Can you explain why the `rev` function we implemented is inefficient => it creates a new list on each iteration.
      Can you write a more efficient version using an accumulating argument?
      What is the efficiency in terms of time taken and space used? => constant
*)

let rev_tr lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | x :: xs -> inner xs (x :: acc)
  in
  inner lst []

let%test _ = rev_tr [] = []
let%test _ = rev_tr [ 1; 2; 3 ] = [ 3; 2; 1 ]
(*
rev_tr    [ 1; 2; 3 ]
 ~> inner [ 1; 2; 3 ] []
 ~> inner [ 2; 3 ] 1 :: []
 ~> inner [ 3 ] 2 :: 1 :: []
 ~> inner [] 3 :: 2 :: 1 :: []
 ~> inner 3 :: 2 :: 1 :: []
 ~> [ 3; 2; 1 ]
*)
