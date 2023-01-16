(* Lib.Chapter13.run ();; *)

(* Verbose version *)
let swap a b =
  let tmp = !a in
  let () = a := !b in
  let () = b := tmp in
  ()
;;

(* Terse version with the semicolon operator *)
let swap' a b =
  let tmp = !a in
  a := !b
  ; b := tmp
;;

let run () =
  (* `x` is a mutable variable, an "int ref" *)
  let x = ref 0 in
  (* Its contents can be extracted with the `!` operator *)
  let () = Printf.printf "x=%d\n" !x in
  let p = !x in
  let () = Printf.printf "p=%d\n" p in
  (* Its contents can be modified with the `:=` operator *)
  let () = x := 50 in
  let q = !x in
  let () = Printf.printf "x=%d (changed)\n" !x in
  let () = Printf.printf "p=%d (no change since it's a value)\n" p in
  let () = Printf.printf "q=%d\n" q in
  (* Swapping *)
  let y = ref 100 in
  let () = Printf.printf "x=%d, y=%d (before swap)\n" !x !y in
  let () = swap x y in
  let () = Printf.printf "x=%d, y=%d (after swap)\n" !x !y in
  (* If the expression of the first branch returns unit, we can omit the second branch *)
  let () =
    if !x > 999 then
      x := 0
    else
      ()
  in
  let () = if !x > 999 then x := 0 in
  let () =
    (* Adding parentheses around the first branch may be required when using the semicolon operator *)
    if !x > 999 then
      let () = x := 0 in
      let () = y := 0 in
      ()
  in
  let () =
    (* Adding parentheses around the first branch may be required when using the semicolon operator *)
    if !x > 999 then (
      x := 0
      ; y := 0
    ) else
      y := 50
  in

  let () =
    (* Adding parentheses around the first branch may be required when using the semicolon operator *)
    if !x > 999 then
      begin
        x := 0
        ; y := 0
      end
    else
      y := 50
    in
    () [@@ocamlformat "disable"]

let run2 () =
  for x = 1 to 5 do
    print_int x
    ; print_newline ()
  done
;;

let smallest_pow2 x =
  let t = ref 1 in
  while !t < x do
    t := !t * 2
  done
  ; !t
;;

let%test_unit _ = [%test_eq: Base.int] 8 @@ smallest_pow2 7
let%test_unit _ = [%test_eq: Base.int] 8 @@ smallest_pow2 8
let%test_unit _ = [%test_eq: Base.int] 16 @@ smallest_pow2 9
let%test_unit _ = [%test_eq: Base.int] 16 @@ smallest_pow2 15
let%test_unit _ = [%test_eq: Base.int] 16 @@ smallest_pow2 16
let%test_unit _ = [%test_eq: Base.int] 32 @@ smallest_pow2 17

let print_histogram arr =
  print_string "Character frequency:"
  ; print_newline ()
  ; for x = 0 to 255 do
      let cnt = arr.(x) in
      if cnt > 0 then (
        print_string "For character '"
        ; print_char (char_of_int x)
        ; print_string "' (character number "
        ; print_int x
        ; print_string ") the count is "
        ; print_int cnt
        ; print_string "."
        ; print_newline ()
      )
    done
;;

let channel_stats chan =
  let lines = ref 0 in
  let chars = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  let histogram = Array.make 256 0 in
  try
    while true do
      let line = input_line chan in
      lines := !lines + 1
      ; chars := !chars + String.length line
      ; line
        |> String.iter @@ fun c ->
           let () =
             (* Update histogram *)
             let i = int_of_char c in
             histogram.(i) <- histogram.(i) + 1
           in
           (* Update other stats *)
           match c with
           | '.'
           | '?'
           | '!' ->
               sentences := !sentences + 1
           | ' ' -> words := !words + 1
           | _ -> ()
    done
  with
  | End_of_file ->
      print_string "There were "
      ; print_int !lines
      ; print_string " lines, making up "
      ; print_int !chars
      ; print_string " characters with "
      ; print_int !words
      ; print_string " words in "
      ; print_int !sentences
      ; print_string " sentences."
      ; print_newline ()
      ; print_histogram histogram
;;

let file_stats name =
  let chan = open_in name in
  try
    channel_stats chan
    ; close_in chan
  with
  | _ -> close_in chan
;;

let%test_unit "side effect" =
  let path =
    (* I'm somewhere inside _build/ *)
    "../../../../../lib/chapter13.txt"
  in
  [%test_eq: Base.unit] () @@ file_stats path
;;

let array_stuff () =
  (* Initialize an array *)
  let a = [| 1; 2; 3; 4; 5 |] in

  (* Access the first element *)
  let a0 = a.(0) in
  let () = assert (1 = a0) in

  (* Update an item *)
  let () =
    a.(0) <- 99
    ; let a0 = a.(0) in
      assert (99 = a0)
  in

  (* Exception: Invalid_argument "index out of bounds". *)
  (* let a5 = a.(5) in *)
  let () =
    (* Get the length of the array (constant time since it's known at creation time) *)
    print_string "Array length is: "
    ; print_int (Array.length a)
    ; print_newline ()
  in

  (* `Array.make x v` generate x items with v value *)
  (* AAAAA *)

  (*
  `c` has a gotcha below!! Use `Array.make_matrix` instead!

  let arr = Array.make 2 (Array.make 3 4);;
  val arr : int array array = [|[|4; 4; 4|]; [|4; 4; 4|]|]

  arr.(0).(0) <- 1;;
  - : int array array = [|[|1; 4; 4|]; [|1; 4; 4|]|]

  *)
  let () =
    let a = Array.make 5 'A' in
    let b = Array.make 5 2 in
    let c = Array.make 2 (Array.make 3 4) in
    assert ([| 'A'; 'A'; 'A'; 'A'; 'A' |] = a)
    ; assert ([| 2; 2; 2; 2; 2 |] = b)
    ; assert ([| [| 4; 4; 4 |]; [| 4; 4; 4 |] |] = c)
  in
  ()
;;

let for' from upto f =
  let rec inner from upto =
    if from <= upto then (
      f
      ; inner (from + 1) upto
    )
  in
  assert (from < upto)
  ; inner from upto
;;

(* QUESTIONS *)

(*
  1. Consider the expression:

      let x = ref 1 in
      let y = ref 2 in
        x := !x + !x
      ; y := !x + !y
      ; !x + !y

    What references have been created? => 2
    What are their initial and final values => (1, 2) => (2, 4)
    What is the type of this (last) expression? => int

  2. What is the difference between `[ref 5; ref 5]` and `let x = ref 5 in [x;x]`?
      The first list contains two references, whereas the first one references a unique ref, twice.

  3. Imagine that the `for.. to .. do .. done` construct did not exist.
     How might we create the same behavior? => as in `for'` above

  4. What are the types of these expressions?

  - int array
  - bool array
  - int array array
  - int list array
  - int
  - unit
*)

(*
  5. Write a function to compute the sum of the elements in an integer array.
*)

let sum (arr : 'a array) =
  let rec loop n acc =
    if n > 0 then
      loop (n - 1) (acc + arr.(n - 1))
    else
      acc
  in
  loop (Array.length arr) 0
;;

let%test_unit _ = [%test_eq: Base.int] 0 @@ sum [||]
let%test_unit _ = [%test_eq: Base.int] 1 @@ sum [| 1 |]
let%test_unit _ = [%test_eq: Base.int] 3 @@ sum [| 1; 2 |]
let%test_unit _ = [%test_eq: Base.int] 6 @@ sum [| 1; 2; 3 |]

(*
  6. Write a function to reverse the elements of an array in place (i.e. do not create a new array).
*)

let reverse arr =
  let len = Array.length arr in

  if len > 1 then
    for a = 0 to (len / 2) - 1 do
      let tmp = arr.(a) in
      let z = len - a - 1 in
      arr.(a) <- arr.(z)
      ; arr.(z) <- tmp
    done
  ; arr
;;

let arr_eq a b = [%test_eq: Base.int Base.array] a b

let%test_unit _ = arr_eq [||] @@ reverse [||]
let%test_unit _ = arr_eq [| 1 |] @@ reverse [| 1 |]
let%test_unit _ = arr_eq [| 2; 1 |] @@ reverse [| 1; 2 |]
let%test_unit _ = arr_eq [| 3; 2; 1 |] @@ reverse [| 1; 2; 3 |]
let%test_unit _ = arr_eq [| 4; 3; 2; 1 |] @@ reverse [| 1; 2; 3; 4 |]
let%test_unit _ = arr_eq [| 5; 4; 3; 2; 1 |] @@ reverse [| 1; 2; 3; 4; 5 |]

(*
  7. Write a function `table` which, given an integer, builds the `int array array`
     representing the multiplication table up to that number.

     For example, `table 5` should yield:

     1   2   3   4   5
     2   4   6   8   10
     3   6   9   12  15
     4   8   12  16  20
     5   10  15  20  25
*)

let table n =
  let arr = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      arr.(i).(j) <- (i + 1) * (j + 1)
    done
  done
  ; arr
;;

let%expect_test _ =
  let open Core in
  print_s [%sexp (table 5 : int array array)]
  ; [%expect
      {| ((1 2 3 4 5) (2 4 6 8 10) (3 6 9 12 15) (4 8 12 16 20) (5 10 15 20 25)) |}]
;;

(*
  8. The ASCII codes for the lower case letters 'a'...'z' are 97..122, and for the upper
     case letters 'A'...'Z' they are 65..90.

     Use the built-in functions `int_of_char` and `char_of_int` to write functions to
     uppercase and lowercase a character.

     Non alphabetic characters should remain unaltered.
*)

let is_alpha c =
  let ic = int_of_char c in
  (ic >= 65 && ic <= 90) || (ic >= 97 && ic <= 122)
;;

let lower c =
  if is_alpha c then
    int_of_char c + (97 - 65) |> char_of_int
  else
    c
;;

let lower s = s |> String.to_seq |> Seq.map lower |> String.of_seq

let%expect_test _ =
  let open Core in
  print_s [%sexp (lower "ABC" : string)]
  ; print_s [%sexp (lower "A-B-C" : string)]
  ; [%expect {|
    abc
    a-b-c |}]
;;

let upper c =
  if is_alpha c then
    int_of_char c - (97 - 65) |> char_of_int
  else
    c
;;

let upper s = s |> String.to_seq |> Seq.map upper |> String.of_seq

type expect = { want : string; got : string }

let expect_string =
  let open Base in
  List.map ~f:(fun expect -> [%test_eq: string] expect.want expect.got)
;;

let (_ : unit list) =
  expect_string
    [ { want = "abcd"; got = lower "ABCD" }
    ; { want = "a.*d"; got = lower "A.*D" }
    ; { want = "EFGH"; got = upper "efgh" }
    ; { want = "E<>H"; got = upper "e<>h" }
    ]
;;

(*
  9. Comment on the accuracy of our character, word, line, and sentence statistics in the
     case of our example paragraph. What about it in general?

     N/A
*)

(*
  10. Choose one of the problems you have identified, and modify our program to fix it

    Skipping, not interesting.
*)
