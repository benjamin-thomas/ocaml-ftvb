type color = Red | Green | Blue | Yellow | RGB of int * int * int

let col = Blue
let cols = [ Red; Green; Blue; Yellow ]
let colpair = ('R', Red)

let components c =
  match c with
  | Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | Yellow -> (255, 255, 0)
  | RGB (r, g, b) -> (r, g, b)

(* `option` is a "polymorphic type" *)
type 'a option = None | Some of 'a

let nothing = None
let number = Some 1
let numbers = [ Some 1; None; None; Some 4 ]
let word = Some [ 'H'; 'e'; 'l'; 'l'; 'o' ]

let rec lookup k dict =
  match dict with
  | [] -> None
  | (k', v) :: t ->
      if k = k' then
        Some v
      else
        lookup k t

let%test _ = [ (1, "Hello"); (2, "World") ] |> lookup 1 = Some "Hello"
let%test _ = [ (1, "Hello"); (2, "World") ] |> lookup 2 = Some "World"
let%test _ = [ (1, "Hello"); (2, "World") ] |> lookup 3 = None

(* In addition to polymorphism, types can also be recursive
   `sequence` here mimics the builtin `list` type *)

type 'a sequence = Nil | Cons of 'a * 'a sequence

(* equivalent to the builtin: [] *)
let empty_list = Nil

(* equivalent to the builtin: [1] *)
let one_item = Cons (1, Nil)

(* equivalent to: [1;2] *)
let two_items : int sequence = Cons (1, Cons (2, Nil))

(* Usage of two custom types *)

(* red, green, blue, cyan *)
let a_color_sequence =
  Cons (Red, Cons (Green, Cons (Blue, Cons (RGB (85, 255, 255), Nil))))

let rec length lst =
  match lst with
  | [] -> 0
  | _ :: t -> 1 + length t

let%test _ = 0 = length []
let%test _ = 1 = length [ 1 ]
let%test _ = 2 = length [ 1; 2 ]
let%test _ = 3 = length [ 1; 2; 3 ]

let rec length' seq =
  match seq with
  | Nil -> 0
  | Cons (_a, b) -> 1 + length' b

let%test _ = 0 = length' @@ Nil
let%test _ = 1 = length' @@ Cons (1, Nil)
let%test _ = 2 = length' @@ Cons (1, Cons (2, Nil))
let%test _ = 3 = length' @@ Cons (1, Cons (2, Cons (3, Nil)))
let%test _ = 3 = length  @@ 1 :: 2 :: 3 :: [] [@@ocamlformat "disable"]

let rec append a lst =
  match lst with
  | [] -> a :: lst
  | h :: t -> h :: append a t

let%test _ = append 1 [] = [ 1 ]
let%test _ = append 2 [ 1 ] = [ 1; 2 ]
let%test _ = append 3 [ 1; 2 ] = [ 1; 2; 3 ]

let rec append' a seq =
  match seq with
  | Nil -> Cons (a, Nil)
  | Cons (h, t) -> Cons (h, append' a t)

let%test _ = append' 1 Nil = Cons (1, Nil)
let%test _ = append' 2 @@ Cons (1, Nil) = Cons (1, Cons (2, Nil))

let%test _ =
  append' 3 @@ Cons (1, Cons (2, Nil)) = Cons (1, Cons (2, Cons (3, Nil)))

(* The book called this function "append" *)
let rec join a b =
  match a with
  | [] -> b
  | h :: t -> h :: join t b

let%test _ = join [ 1 ] [] = [ 1 ]
let%test _ = join [] [ 1 ] = [ 1 ]
let%test _ = join [ 1 ] [ 2 ] = [ 1; 2 ]
let%test _ = join [ 1; 2 ] [ 3 ] = [ 1; 2; 3 ]

let rec join' a b =
  match a with
  | Nil -> b
  | Cons (h, t) -> Cons (h, join' t b)

let%test _ = join' Nil Nil = Nil
let%test _ = join' (Cons (1, Nil)) Nil = Cons (1, Nil)
let%test _ = join' Nil (Cons (1, Nil)) = Cons (1, Nil)
let%test _ = join' (Cons (1, Nil)) (Cons (2, Nil)) = Cons (1, Cons (2, Nil))

let%test _ =
  join' (Cons (1, Cons (2, Nil))) (Cons (3, Nil))
  = Cons (1, Cons (2, Cons (3, Nil)))

type expr =
  | Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mlt of expr * expr
  | Div of expr * expr
  | Pow of expr * expr

(* Represents: 1+2*3 *)
let my_operation = Add (Num 1, Mlt (Num 2, Num 3))

(* OMG! *)
let rec eval e =
  match e with
  | Num x -> x
  | Add (a, b) -> eval a + eval b
  | Sub (a, b) -> eval a - eval b
  | Mlt (a, b) -> eval a * eval b
  | Div (a, b) -> eval a / eval b
  | Pow (a, n) ->
      let rec inner a n =
        match n with
        | 0 -> 1
        | _ ->
            if n > 0 then
              a * inner a (n - 1)
            else
              failwith
                "I don't handle negative exponents! I would have to return \
                 floats!"
      in
      inner (eval a) (eval n)

let%test _ = eval @@ Add (Num 1, Num 2) = 3
let%test _ = eval @@ Sub (Num 10, Num 2) = 8
let%test _ = eval @@ Mlt (Num 3, Num 4) = 12
let%test _ = eval @@ Div (Num 12, Num 4) = 3
let%test _ = eval my_operation = 7

(* 1. Design a new type `rect` for representing rectangles.
      Treat squares as a special case *)

open Sexplib.Std (* derive `int` *)

type width = Width of int [@@deriving sexp, ord]
type length = Length of int [@@deriving sexp, ord]
type side = Side of int [@@deriving sexp, ord]

type rect = Rectangle of width * length | Square of side
[@@deriving sexp, ord]

(* 2. Now write a function of type `rect -> int` to calculate the area of a given rect *)

let area r =
  match r with
  | Rectangle (Width w, Length l) -> w * l
  | Square (Side s) -> s * s

let%test _ = area @@ Square (Side 5) = 25
let%test _ = area @@ Rectangle (Width 3, Length 4) = 12

(* 3. Write a function which rotates a `rect` such that it is at least as tall as wide *)

let rotate r =
  match r with
  | Square _ as sq -> sq
  | Rectangle (Width a, Length b) -> Rectangle (Width b, Length a)

(* 4. Use this function to write one which, given a `rect list`, returns another such list
      which has the smallest total width and whose members are sorted narrowest first. *)

let width_length r =
  match r with
  | Square (Side s) -> (Width s, Length s)
  | Rectangle (Width w, Length l) -> (Width w, Length l)

let rotate_to_narrowest r =
  match r with
  | Square _ as sq -> sq
  | Rectangle (Width w, Length l) ->
      if l < w then
        rotate r
      else
        r

let rec sort_rect lst : rect list =
  let rec insert a lst =
    match lst with
    | [] -> [ a ]
    | h :: t ->
        let h = rotate_to_narrowest h in
        let a = rotate_to_narrowest a in
        let Width aw, _ = width_length a in
        let Width hw, _ = width_length h in
        if aw <= hw then
          a :: insert h t
        else
          h :: insert a t
  in
  match lst with
  | [] -> []
  | h :: t -> insert h (sort_rect t)

let%test_unit _ = [%test_eq: rect Base.list] [] @@ sort_rect []

let%test_unit _ =
  [%test_eq: rect Base.list] [ Rectangle (Width 1, Length 2) ]
  @@ sort_rect [ Rectangle (Width 1, Length 2) ]

let%test_unit _ =
  [%test_eq: rect Base.list]
    [ Rectangle (Width 1, Length 2); Rectangle (Width 3, Length 4) ]
  @@ sort_rect [ Rectangle (Width 3, Length 4); Rectangle (Width 1, Length 2) ]

let%test_unit "sort by narrowest width" =
  [%test_eq: rect Base.list]
    [
      Rectangle (Width 1, Length 3);
      Rectangle (Width 2, Length 5);
      Rectangle (Width 3, Length 6);
      Square (Side 4);
    ]
  @@ sort_rect
       [
         Rectangle (Width 5, Length 2);
         Square (Side 4);
         Rectangle (Width 3, Length 6);
         Rectangle (Width 1, Length 3);
       ]

(* 5. Write `take`, `drop` and `map` functions for the sequence type *)

(* TAKE *)
let rec take n seq =
  if n = 0 then
    Nil
  else
    match seq with
    | Nil -> failwith "Invalid arg: `n` must be greater or equal to seq length!"
    | Cons (h, t) -> Cons (h, take (n - 1) t)

let%test _ = Nil = take 0 Nil
let%test _ = Nil = take 0 (Cons (1, Nil))
let%test _ = Cons (1, Nil) = take 1 (Cons (1, Nil))
let%test _ = Cons (1, Nil) = take 1 (Cons (1, Cons (2, Cons (3, Nil))))

let%test _ =
  Cons (1, Cons (2, Nil)) = take 2 (Cons (1, Cons (2, Cons (3, Nil))))

let%expect_test _ =
  Expect_test_helpers_core.require_does_raise [%here] (fun () -> take 1 Nil);
  [%expect
    {| (Failure "Invalid arg: `n` must be greater or equal to seq length!") |}]

let%expect_test _ =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      take 2 (Cons (1, Nil)));
  [%expect
    {| (Failure "Invalid arg: `n` must be greater or equal to seq length!") |}]

(* DROP *)
let rec drop n seq =
  if n < 0 then
    failwith "Invalid arg: `n` must be greater than or equal to 0!"
  else if n = 0 then
    seq
  else
    match seq with
    | Nil ->
        if n > 1 then
          Nil
        else
          failwith "Invalid arg: `n` must be less than or equal to seq length!"
    | Cons (_, t) -> drop (n - 1) t

let%expect_test _ =
  Expect_test_helpers_core.require_does_raise [%here] (fun () -> drop (-1) Nil);
  [%expect
    {| (Failure "Invalid arg: `n` must be greater than or equal to 0!") |}]

let%test _ =
  Cons (1, Cons (2, Cons (3, Nil))) = drop 0 (Cons (1, Cons (2, Cons (3, Nil))))

let%test _ =
  Cons (2, Cons (3, Nil)) = drop 1 (Cons (1, Cons (2, Cons (3, Nil))))

let%test _ = Cons (3, Nil) = drop 2 (Cons (1, Cons (2, Cons (3, Nil))))
let%test _ = Nil = drop 3 (Cons (1, Cons (2, Cons (3, Nil))))

let%expect_test _ =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      drop 4 (Cons (1, Cons (2, Cons (3, Nil)))));
  [%expect
    {| (Failure "Invalid arg: `n` must be less than or equal to seq length!") |}]

(* MAP *)

let rec map f seq =
  match seq with
  | Nil -> Nil
  | Cons (h, t) -> Cons (f h, map f t)

let%test _ = Nil = map rotate Nil

let%test _ =
  Cons (Square (Side 1), Nil) = map rotate @@ Cons (Square (Side 1), Nil)

let%test _ =
  Cons (Rectangle (Width 4, Length 3), Nil)
  = map rotate @@ Cons (Rectangle (Width 3, Length 4), Nil)

(* 6. Extend the `expr` type and the `eval` function to allow raising a number to a power *)

let%test _ = eval @@ Pow (Num 10, Num 0) = 1
let%test _ = eval @@ Pow (Num 10, Num 1) = 10
let%test _ = eval @@ Pow (Num 10, Num 2) = 100
let%test _ = eval @@ Pow (Num 10, Num 3) = 1000

(* 7. Use the `option` type to deal with the problem that `Division_by_zero` may be raised
      from the `eval` function *)

let eval_opt x =
  try Some (eval x) with
  | Division_by_zero -> None

let%test _ = eval_opt @@ Div (Num 10, Num 0) = None
let%test _ = eval_opt @@ Div (Num 10, Num 2) = Some 5
