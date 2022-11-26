(*
   utop -init chapter02/main.ml

   #use "chapter02/main.ml";
*)

(* 1. Write a function which multiplies a given number by ten. What is its type. *)
let by10 n = n * 10

(* 2. Write a function that returns true if both of its arguments are non-zero, and false otherwise *)
let both_non_zero a b = a <> 0 && b <> 0

(* 3. Write a recursive function which, given a number `n`, calculates the sum 1 + 2 + 3 + ... + `n` *)
let rec add_series n = if n = 1 then 1 else n + add_series (n - 1)

(* 4. Write a function `power x n` wich raises x to the power n *)
let rec power x n = if n = 0 then 1 else x * power x (n - 1)

(* 5. Write a function `isconsonant` which, given a lower-case character in the range 'a'..'z',
      determines if it is a consonant *)
let isconsonant c =
  let isvowel = 'a' = c || 'e' = c || 'i' = c || 'o' = c || 'u' = c in
  not isvowel
;;
