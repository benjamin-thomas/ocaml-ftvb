let ( == ) a b = [%test_eq: Base.int Base.list] a b
let raises fn = Expect_test_helpers_core.require_does_raise [%here] fn

let rec take n lst =
  match lst with
  | [] ->
      if n = 0 then
        []
      else
        raise @@ Invalid_argument "take"
  | x :: xs ->
      if n = 0 then
        []
      else
        x :: take (n - 1) xs

let%test_unit _ = take 0 [ 1; 2; 3 ] == []
let%test_unit _ = take 1 [ 1; 2; 3 ] == [ 1 ]
let%test_unit _ = take 2 [ 1; 2; 3 ] == [ 1; 2 ]
let%test_unit _ = take 3 [ 1; 2; 3 ] == [ 1; 2; 3 ]

let%expect_test _ =
  raises (fun () -> take 4 [ 1; 2; 3 ]);
  [%expect {| (Invalid_argument take) |}]

let%expect_test _ =
  raises (fun () -> take (-1) [ 1; 2; 3 ]);
  [%expect {| (Invalid_argument take) |}]

let rec drop n lst =
  match lst with
  | [] ->
      if n = 0 then
        []
      else
        raise @@ Invalid_argument "drop"
  | _x :: xs ->
      if n = 0 then
        lst
      else
        drop (n - 1) xs

let%test_unit _ = drop 0 [ 1; 2; 3 ] == [ 1; 2; 3 ]
let%test_unit _ = drop 1 [ 1; 2; 3 ] == [ 2; 3 ]
let%test_unit _ = drop 2 [ 1; 2; 3 ] == [ 3 ]
let%test_unit _ = drop 3 [ 1; 2; 3 ] == []

let%expect_test _ =
  raises (fun () -> drop 4 [ 1; 2; 3 ]);
  [%expect {| (Invalid_argument drop) |}]

let%expect_test _ =
  raises (fun () -> drop (-1) [ 1; 2; 3 ]);
  [%expect {| (Invalid_argument drop) |}]

exception Forbidden_id of int

let get id =
  if id <= 0 || id = 8 then
    raise @@ Forbidden_id id
  else
    (id, id * 2, "data")

let%expect_test "sub" =
  let open Core in
  let open Base in
  print_s [%sexp (get 4 : int * int * string)];
  [%expect {| (4 8 data) |}]

let%expect_test "Handle special number 8" =
  raises (fun () -> get 8);
  [%expect {| ("Lib__Chapter07.Forbidden_id(8)") |}]

let%expect_test "Handle zero" =
  raises (fun () -> get 0);
  [%expect {| ("Lib__Chapter07.Forbidden_id(0)") |}]

let%expect_test "Handle negative values" =
  raises (fun () -> get (-99));
  [%expect {| ("Lib__Chapter07.Forbidden_id(-99)") |}]

let%expect_test _ =
  raises (fun () -> 1 / 0);
  [%expect {|(Division_by_zero)|}]

let safe_div a b =
  try a / b with
  | Division_by_zero -> 0

let%expect_test _ =
  Core.print_s [%sexp (safe_div 1 0 : Base.int)];
  [%expect {| 0 |}]

(* Questions *)

(* 1. Write a function `smallest` which returns the smallest positive element of
   a list of integers. If there is no positive element, it should raise the
   build-in Not_found exception.
*)

let smallest lst =
  let rec inner lst found =
    match lst with
    | [] ->
        if found < 0 then
          raise Not_found
        else
          found
    | x :: xs ->
        if x >= 0 && x < found then
          inner xs x
        else
          inner xs found
  in
  if List.length lst = 0 then
    raise Not_found
  else
    inner lst (List.hd lst)

let%expect_test _ =
  Core.print_s [%sexp (smallest [ 4; 2; -1 ] : Base.int)];
  [%expect {| 2 |}]

let%expect_test _ =
  raises (fun () -> smallest []);
  [%expect {| Not_found |}]

let%expect_test _ =
  raises (fun () -> smallest [ -1; -2 ]);
  [%expect {| Not_found |}]

(* 2. Write another function `smallest_or_zero` which uses the `smallest`
   function but if `Not_found` is raised, returns zero *)

let smallest_or_zero lst =
  try smallest lst with
  | Not_found -> 0

let%expect_test _ =
  Core.print_s [%sexp (smallest_or_zero [ -1; -2 ] : Base.int)];
  [%expect {| 0 |}]

(* 3. Write an exception definition and a function which calculates the largest
   integer smaller than or equal to the square root of a given integer. If the
   argument is negative, the exception should be raised.


   sqrt(49) = 7
   sqrt(50) = approx. 7.0710678
*)

exception Invalid_neg_arg

let largest_upto_sqrt n =
  if n < 0 then
    raise Invalid_neg_arg
  else
    float_of_int n |> sqrt |> int_of_float

let%expect_test _ =
  Core.print_s [%sexp (largest_upto_sqrt 25 : Base.int)];
  [%expect {| 5 |}]

let%expect_test _ =
  Core.print_s [%sexp (largest_upto_sqrt 50 : Base.int)];
  [%expect {| 7 |}]

let%expect_test _ =
  raises (fun () -> largest_upto_sqrt (-1));
  [%expect {| (Lib__Chapter07.Invalid_neg_arg) |}]

(* 4. Write another function which uses the previous one, but handles the
   exception, and simply returns zero when a suitable integer cannot be found. *)

let largest_upto_sqrt_or_zero n =
  try largest_upto_sqrt n with
  | Invalid_neg_arg -> 0

let%expect_test _ =
  Core.print_s [%sexp (largest_upto_sqrt_or_zero (-1) : Base.int)];
  [%expect {| 0 |}]

(* 5. Comment on the merits and demerits of execptions as a method for dealing
   with exceptional situations, in contrast to returning a special value to
   indicate an error (such as -1 for a function normally returning a positive
   number)

   + simplifies potentially complex (or verbose) error handling
    + could be reasonable if assumptions for the function inputs are correct
   - hides error handling
   - type sig becomes a lie
   - type system can't help handle all exceptional situations
*)
