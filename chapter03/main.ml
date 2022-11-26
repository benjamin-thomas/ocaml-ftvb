(*
   while true;do utop -init ./chapter03/main.ml;done

   #use "chapter03/main.ml";
*)

(*
   WITHOUT PATTERN MATCHING
   ========================
*)

let rec factorial n = if n = 1 then 1 else n * factorial (n - 1)
let isvowel c = 'a' = c || 'e' = c || 'i' = c || 'o' = c || 'u' = c

(* Euclid's Algorithm *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(*
   WITH PATTERN MATCHING
   =====================
*)
let rec factorial' n =
  match n with
  | 1 -> 1
  | _ -> n * factorial (n - 1)
;;

let isvowel' c =
  match c with
  | 'a' -> true
  | 'e' -> true
  | 'i' -> true
  | 'o' -> true
  | _ -> false
;;

let isvowel'' c =
  match c with
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false
;;

let rec gcd' a b =
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)
;;

(*
   EXERCISES
   =========
*)

(* 1. Rewrite the `not` function from the previous chapter in pattern matching style *)
let not' a =
  match a with
  | true -> false
  | false -> true
;;

(* 2. Use pattern matching to write a recursive function which, given a positive
   integer `n`, returns the sum of all integers from 1 to `n` *)
let rec sum n =
  match n with
  | 0 -> n
  | _ -> n + sum (n - 1)
;;

(* 3. Use pattern matching to write a function which, given two numbers `x` and `n`, computes `x to the power n` *)
let rec pow x n =
  match n with
  | 0 -> 1
  | _ -> x * pow x (n - 1)
;;

let is_lower c =
  match c with
  | 'a' .. 'z' -> true
  | _ -> false
;;

(*$T is_upper
is_upper 'c' = false
is_upper 'A' = true
is_upper '1' = false
*)
let is_upper c =
  match c with
  | 'A' .. 'Z' -> true
  | _ -> false
;;
