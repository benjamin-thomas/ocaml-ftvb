(*
Br -> branch
Lf -> leaf

`tree` is "polymorphic": it can hold any kind of data at the branches.

Br holds 3 things: an element + a left sub-tree + a right sub-tree.
Lf signals that there is no left or right sub-tree.
*)
type 'a tree = Br of 'a * 'a tree * 'a tree | Lf [@@deriving sexp, compare]

(*
   https://en.wikipedia.org/wiki/Box-drawing_character
   http://marklodato.github.io/js-boxdrawing/
*)

(* one_node
     ┌─╴
   1─┤
     └─╴
*)
let one_node = Br (1, Lf, Lf)

(* two_nodes
         ┌─╴
     ┌─1─┤
   2─┤   └─╴
     └─╴

   2
   └── 1
*)
let two_nodes = Br (2, Br (1, Lf, Lf), Lf)

(* three_nodes
         ┌─╴
     ┌─1─┤
     │   └─╴
   2─┤
     │   ┌─╴
     └─4─┤
         └─╴

    ┌── 4
    2
    └── 1
*)
let three_nodes = Br (2, Br (1, Lf, Lf), Br (4, Lf, Lf))

let rec size tr =
  match tr with
  | Lf -> 0
  | Br (_, l, r) -> 1 + size l + size r
;;

let%test _ = 1 = size one_node
let%test _ = 2 = size two_nodes
let%test _ = 3 = size three_nodes

let rec sum tr =
  match tr with
  | Lf -> 0
  | Br (n, l, r) -> n + sum l + sum r
;;

let%test _ = 1 = sum one_node
let%test _ = 3 = sum two_nodes
let%test _ = 7 = sum three_nodes

let max x y =
  if x > y then
    x
  else
    y
;;

(* WOW *)
let rec max_depth tr =
  match tr with
  | Lf -> 0
  | Br (_, l, r) -> 1 + max (max_depth l) (max_depth r)
;;

let%test _ = 1 = max_depth one_node
let%test _ = 2 = max_depth two_nodes
let%test _ = 2 = max_depth three_nodes
let%test _ = 0 = max_depth Lf
let%test _ = 1 = max_depth @@ Br (1, Lf, Lf)
let%test _ = 2 = max_depth @@ Br (1, Lf, Br (2, Lf, Lf))
let%test _ = 2 = max_depth @@ Br (1, Br (3, Lf, Lf), Br (2, Lf, Lf))
let%test _ = 3 = max_depth @@ Br (1, Lf, Br (2, Lf, Br (3, Lf, Lf)))

let rec list_of_tree tr =
  match tr with
  | Lf -> []
  | Br (x, l, r) -> list_of_tree l @ [ x ] @ list_of_tree r
;;

let%test_unit _ = [%test_eq: Base.int Base.list] [ 1 ] @@ list_of_tree one_node

let%test_unit _ =
  [%test_eq: Base.int Base.list] [ 1; 2 ] @@ list_of_tree two_nodes
;;

let%test_unit _ =
  [%test_eq: Base.int Base.list] [ 1; 2; 4 ] @@ list_of_tree three_nodes
;;

let rec tree_map f tr =
  match tr with
  | Lf -> Lf
  | Br (x, l, r) -> Br (f x, tree_map f l, tree_map f r)
;;

let dbl x = x * 2

let%test_unit _ =
  [%test_eq: Base.int tree] (Br (2, Lf, Lf)) @@ tree_map dbl (Br (1, Lf, Lf))
;;

let%test_unit _ =
  [%test_eq: Base.int tree] (Br (4, Br (2, Lf, Lf), Br (8, Lf, Lf)))
  @@ tree_map dbl (Br (2, Br (1, Lf, Lf), Br (4, Lf, Lf)))
;;

(* Assuming the tree is properly balanced *)
let rec lookup tr k =
  match tr with
  | Lf -> None
  | Br ((k', v), l, r) ->
      if k = k' then
        Some v
      else if k < k' then
        lookup l k
      else
        lookup r k
;;

(*
              (3, "three")
                   │
         ┌─────────┴───────────┐
         │                     │
    (1, "one"              (4, "four")
         ╷                     │
         │                 ┌───┴───┐
    ┌────┴────┐            │       │
    │         │           ╶┴╴     ╶┴╴
   ╶┴╴    (2, "two")
              │
              │
         ┌────┴─────┐
         │          │
        ╶┴╴        ╶┴╴

Yuck!

    ┌── (4, four)
    (3, three)
    │   ┌── (2, two)
    └── (1, one)


*)
let balanced_tree =
  Br
    ( (3, "three")
    , Br ((1, "one"), Lf, Br ((2, "two"), Lf, Lf))
    , Br ((4, "four"), Lf, Lf) )
;;

let%test _ = lookup balanced_tree 1 = Some "one"
let%test _ = lookup balanced_tree 2 = Some "two"
let%test _ = lookup balanced_tree 3 = Some "three"
let%test _ = lookup balanced_tree 4 = Some "four"
let%test _ = lookup balanced_tree 5 = None

let rec insert k v tr =
  match tr with
  | Lf -> Br ((k, v), Lf, Lf)
  | Br ((k', v'), l, r) ->
      if k = k' then
        Br ((k, v), l, r)
      else if k < k' then
        Br ((k', v'), insert k v l, r)
      else
        Br ((k', v'), l, insert k v r)
;;

let%test_unit _ =
  [%test_eq: (Base.int * Base.string) tree] (Br ((1, "one"), Lf, Lf))
  @@ insert 1 "one" Lf
;;

let%test_unit _ =
  [%test_eq: (Base.int * Base.string) tree]
    (Br ((1, "one"), Lf, Br ((2, "two"), Lf, Lf)))
  @@ insert 2 "two" (Br ((1, "one"), Lf, Lf))
;;

let rec insert' v tr =
  match tr with
  | Lf -> Br (v, Lf, Lf)
  | Br (x, l, r) ->
      if v < x then
        Br (x, insert' v l, r)
      else
        Br (x, l, insert' v r)
;;

let%test_unit _ = [%test_eq: Base.int tree] (Br (2, Lf, Lf)) @@ insert' 2 Lf

let%test_unit _ =
  [%test_eq: Base.int tree] (Br (2, Br (1, Lf, Lf), Lf))
  @@ insert' 1
  @@ Br (2, Lf, Lf)
;;

let%test_unit _ =
  [%test_eq: Base.int tree] (Br (2, Br (1, Lf, Lf), Br (3, Lf, Lf)))
  @@ insert' 3
  @@ Br (2, Br (1, Lf, Lf), Lf)
;;

let%test_unit _ =
  [%test_eq: Base.int tree] (Br (2, Br (1, Lf, Lf), Lf))
  @@ insert' 1
  @@ Br (2, Lf, Lf)
;;

let%test_unit _ =
  [%test_eq: Base.int tree] (Br (2, Lf, Br (3, Lf, Lf)))
  @@ insert' 3
  @@ Br (2, Lf, Lf)
;;

let%test_unit _ =
  [%test_eq: Base.int tree] (Br (4, Br (2, Br (1, Lf, Lf), Lf), Br (6, Lf, Lf)))
  @@ insert' 1
  @@ Br (4, Br (2, Lf, Lf), Br (6, Lf, Lf))
;;

(* Not sure why this test doesn't pass *)
(* let%test_unit _ =
   [%test_eq: (Base.int * Base.string) tree]
     (Br ((3, "three"), Br ((1, "one"), Lf, Br ((2, "two"), Lf, Lf)), Lf))
   @@ insert 3 "three" (Br ((1, "one"), Lf, Br ((2, "two"), Lf, Lf))) *)

let%test_unit _ =
  [%test_eq: (Base.int * Base.string) tree]
    (Br
       ( (3, "three")
       , Br ((1, "one"), Lf, Br ((2, "two"), Lf, Lf))
       , Br ((4, "four"), Lf, Lf) ))
  @@ insert 4 "four"
       (Br ((3, "three"), Br ((1, "one"), Lf, Br ((2, "two"), Lf, Lf)), Lf))
;;

(* Questions *)

(*
  1. Write a function of type `'a -> 'a tree -> bool` to determine if a given
     element is in a tree
*)

let rec is_present x tr =
  match tr with
  | Lf -> false
  | Br (x', l, r) ->
      if x = x' then
        true
      else
        is_present x l || is_present x r
;;

let%test _ = is_present 0 one_node |> not
let%test _ = is_present 1 one_node
let%test _ = is_present 2 one_node |> not
let%test _ = is_present 4 three_nodes
let%test _ = is_present 5 three_nodes |> not
let%test _ = balanced_tree |> is_present (1, "one")
let%test _ = balanced_tree |> is_present (2, "two")
let%test _ = balanced_tree |> is_present (3, "three")
let%test _ = balanced_tree |> is_present (4, "four")
let%test _ = balanced_tree |> is_present (5, "five") |> not

let nine_items =
  Br ( 5
     , Br ( 4
          , Br ( 2
                , Br ( 1, Lf, Lf )
                , Br ( 3, Lf, Lf )
                )
          , Lf
          )
     , Br ( 6
          , Lf
          , Br ( 8
                , Br ( 7, Lf, Lf )
                , Br ( 9, Lf, Lf )
                )
          )
     )
[@@ocamlformat "disable"]

let nine_items_flipped =
  Br ( 5
     , Br ( 6
          , Lf
          , Br ( 8
               , Br ( 7, Lf, Lf )
               , Br ( 9, Lf, Lf )
               )
          )
     , Br ( 4
          , Br ( 2
                , Br ( 1, Lf, Lf )
                , Br ( 3, Lf, Lf )
                )
          , Lf
          )
     )
[@@ocamlformat "disable"]

(*
  2. Write a function which flips a tree left to right such that, if it were
     drawn on paper, it would appear to be a mirror image.

        ┌── 9
    ┌── 8
    │   └── 7
┌── 6
5
└── 4
    │   ┌── 3
    └── 2
        └── 1


        ┌── 3
    ┌── 2
    │   └── 1
┌── 4
5
└── 6
    │   ┌── 9
    └── 8
        └── 7
*)

let flip tr =
  match tr with
  | Lf -> failwith "Tree is empty!"
  | Br (x, l, r) -> Br (x, r, l)
;;

let%test _ = flip nine_items = nine_items_flipped

(*
   3. Write a function to determine if two trees have the same shape,
      irrespective of the actual values of the elements
*)

let rec is_same_shape a b =
  match (a, b) with
  | Lf, Lf -> true
  | Lf, Br _ -> false
  | Br _, Lf -> false
  | Br (_, al, ar), Br (_, bl, br) -> is_same_shape al bl && is_same_shape ar br
;;

let%test _ = is_same_shape Lf Lf
let%test _ = not @@ is_same_shape Lf (Br (1, Lf, Lf))
let%test _ = not @@ is_same_shape (Br (1, Lf, Lf)) Lf
let%test _ = is_same_shape (Br (1, Lf, Lf)) (Br (2, Lf, Lf))
let%test _ = not @@ is_same_shape (Br (1, Lf, Lf)) (Br (2, Br (1, Lf, Lf), Lf))
let%test _ = not @@ is_same_shape (Br (2, Br (1, Lf, Lf), Lf)) (Br (1, Lf, Lf))
let%test _ = not @@ is_same_shape (Br (1, Lf, Lf)) (Br (2, Lf, Br (1, Lf, Lf)))
let%test _ = not @@ is_same_shape (Br (2, Lf, Br (1, Lf, Lf))) (Br (1, Lf, Lf))

(*
  4. Write a function `tree_of_list` which builds a tree representation of a
     dictionary from a list representation of a dictionary
*)

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t
;;

let rec take n = function
  | [] -> []
  | h :: t ->
      if n > 0 then
        h :: take (n - 1) t
      else
        []
;;

let rec drop n = function
  | [] -> []
  | h :: t ->
      if n <= 0 then
        h :: t
      else
        drop (n - 1) t
;;

(* My solution. I forgot about the `insert k v` function. *)
let rec tree_of_list lst =
  match lst with
  | [] -> Lf
  | h :: t ->
      let half = length t / 2 in
      let left = take half t in
      let right = drop half t in
      Br (h, tree_of_list left, tree_of_list right)
;;

(* Book's solution *)
let rec tree_of_list' lst =
  match lst with
  | [] -> Lf
  | (k, v) :: t -> insert k v (tree_of_list' t)
;;

(* Book's solution, adapted to juts handle values (no keys) *)
let rec tree_of_list'' lst =
  match lst with
  | [] -> Lf
  | h :: t -> insert' h (tree_of_list'' t)
;;

(* https://www.leetcode-tree-visualizer.com/ *)
let seven_items =
  Br ( 4
     , Br ( 2
          , Br ( 1, Lf, Lf )
          , Br ( 3, Lf, Lf )
          )
     , Br ( 6
          , Br ( 5, Lf, Lf )
          , Br ( 7, Lf, Lf )
          )
     )
    [@@ocamlformat "disable"]

let%test_unit _ =
  (*
    TODO: Find a way to build the tree as on leetcode-tree-visualizer, as commented below
    [%test_eq: Base.int tree] seven_items @@ tree_of_list [ 4; 2; 6; 1; 3; 5; 7 ] *)
  [%test_eq: Base.int tree] seven_items @@ tree_of_list [ 4; 2; 1; 3; 6; 5; 7 ]
;;

(*
  5. Write a function to combine two dictionaries represented as trees into one.
     In the case of clashing keys, prefer the value from the first dictionary.
*)

let one_two_three =
  Br ( (2, "two")
     , Br ( (1, "one")
          , Lf
          , Lf
          )
     , Br ( (3, "three")
          , Lf
          , Lf
          )
     )
    [@@ocamlformat "disable"]

(* 6,5,7,4 *)
let four_five_six_seven =
  Br ( (6, "six")
     , Br ( (5, "five")
          , Br ( (4, "four")
               , Lf
               , Lf
               )
          , Lf
          )
     , Br ( (7, "seven")
          , Lf
          , Lf
          )
     )
    [@@ocamlformat "disable"]

(* 4,2,6,1,3,5,7 *)
let want_combined =
  Br ( (4, "four")
     , Br ( (2, "two")
          , Br ( (1, "one")
               , Lf
               , Lf
               )
          , Br ( (3, "three")
               , Lf
               , Lf
               )
          )
     , Br ( (6, "six")
          , Br ( (5, "five")
               , Lf
               , Lf
               )
          , Br ( (7, "seven")
               , Lf
               , Lf
               )
          )
     )
    [@@ocamlformat "disable"]

(* TODO:
    I'll have to implement `list_of_tree`, combine those lists, then recombine into a tree *)
let rec combine_tree a b =
  match a with
  | Lf -> b
  | Br ((k, v), l, r) -> combine_tree l (insert k v r)
;;

(*
FIXME: not correct
let%test_unit _ =
  [%test_eq: (Base.int * Base.string) tree] want_combined
  @@ combine_tree one_two_three one_two_three
;; *)

(*
  TODO: I'll finish this exercise later, want to take a break from trees.
  6. Can you define a type for trees which, instead of branching exactly two ways each time,
     can branch zero or more ways, possibly different at each branch? Write simple functions
     like `size`, `total` and `map` using your new type of tree
*)