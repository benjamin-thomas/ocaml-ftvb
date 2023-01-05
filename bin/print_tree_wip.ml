[@@@warning "-37-27-26-39-32"]

module Tree = struct
  include Lib.Chapter11

  (* type 'a tree = 'a Lib.Chapter11.tree <-- won't have access to the `Br` and `Lf` variants that way *)
  type 'a tree = 'a Lib.Chapter11.tree = Br of 'a * 'a tree * 'a tree | Lf
end

open Tree

(*

echo bin/print_tree.ml | entr -rc dune exec --display=quiet bin/print_tree_wip.exe
rg --files | entr -rc bash -c 'dune exec --display=quiet bin/print_tree_wip.exe'

TODO: I'll have to keep working on printing the tree vertically. And redo the horizontal impl.
TODO: It's an intresting challenge, requires a lot of organization though (can't hack it)
TODO: I want to move on with the book for now.
TODO: Also implement a simple horizontal version like this (this form is **much** easier):

└──8
   ├──5
   │   ├──2
   │   └──6
   └──10
       ├──9
       └──11

*)

(*

┴
┌─
─┐
┼?

  let ten_items =
    Br ( 10
       , Br ( 5
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
        , Lf
        )
  in

  print_newline ();
  print_endline @@ string_of_tree' ten_items;

---

  10
  / \
 2   3
/ \ / \
4 5 6 7

14
7
2
1
                                10
                 ┌──────────────┴──────────────┐
                 5                             x
         ┌───────┴───────┐             ┌───────┴───────┐
         4               6             x               x
      ┌──┴──┐         ┌──┴──┐       ┌──┴──┐         ┌──┴──┐
      2     x         x     8       x     x         x     x
    ┌─┴─┐ ┌─┴─┐     ┌─┴─┐ ┌─┴─┐   ┌─┴─┐ ┌─┴─┐     ┌─┴─┐ ┌─┴─┐
    1   3 -   -     -   - 7   9   -   - -   -     -   - -   -

  10
  │           ┌── 9
  │       ┌── 8
  │       │   └── 7
  │   ┌── 6
  └── 5
      └── 4
          │   ┌── 3
          └── 2
              └── 1


*)

(*

Challenge #1: print them in "vertical order", i.e.

10
5
4 6
2 8
1 3  7 9

*)

let repeat n str =
  let rec inner acc n =
    match n with
    | 0 -> ""
    | 1 -> acc
    | _ -> inner (str ^ acc) (n - 1)
  in
  assert (n >= 0);
  inner str n
;;

let string_of_tree tr =
  let rec inner depth tr =
    let _render_line x : string * int = (string_of_int x, depth) in

    match tr with
    | Lf -> []
    | Br (x, l, r) -> (x, depth) :: List.concat_map (inner (depth + 1)) [ l; r ]
  in

  let item_to_pretty item = "TODO" in

  let by_depth_asc (_, d1) (_, d2) = compare d1 d2 in
  let string_of_item (item, depth) =
    (* let padding = repeat (10 - depth) " " in *)
    let padding = "" in
    Printf.sprintf "%2d" item
  in

  let group_by_depth =
    Core.List.groupi ~break:(fun _ (_, d1) (_, d2) -> d1 <> d2)
  in

  let lines_of_group group = List.map (fun item -> string_of_item item) group in

  inner 0 tr
  |> List.sort by_depth_asc
  |> group_by_depth
  |> List.map (fun group -> "\n" :: lines_of_group group)
  |> List.flatten
  |> String.concat ""
;;

(* |> List.fold_left (fun acc a -> acc ^ string_of_line a) "" *)

(* |> List.fold_left (fun acc a -> acc ^ string_of_line a) "" *)

(* |> List.map string_of_line *)
(* |> String.concat "\n" *)

(*

margin_left

0 -> 13
  -> 13 - 0
  -> 13 - (13-0)
          (13 - 7x2-1 )
1 -> 7
  -> 13 - 6
  -> 13 - (13 - 7)
          (13 - 7x1-1 )
2 -> 0
  -> 13 - 13
  -> 13 - (13 - 7-6)
          (13 - 7x0-1 )

DDDDDDDDDDDDD____01____02
DDDDDDDDDDDDD____/\____/\
7777777______01______02______03
7777777______/\______/\______/\
________01________02________03________04
________/\________/\________/\________/\


https://stackoverflow.com/questions/8964279/coding-a-basic-pretty-printer-for-trees-in-java
(level is 1-based)
Indent  formula : 2^(max_level - level) - 1
Spacing formula : 2^(max level - level) + 1) - 1


       1
   2       3
 4   5   6   7
8 9 A B C D E F
In that tree, the number of levels is 4. The spacing at the last level is 1 whereas the indent is 0. You'll get the following values:

level 1:
- indent = 7: (2^(4-1) - 1 = 2^3 - 1 = 8 - 1 = 7)
- first level, so spacing doesn't matter

level 2:
- indent = 3: (2^(4-2) - 1 = 2^2 - 1 = 4 - 1 = 3)
- spacing = 7 (2^(4-1) - 1 = 2^3 - 1 = 8 - 1 = 7)

level 3:
- indent = 1: (2^(4-3) - 1 = 2^1 - 1 = 2 - 1 = 1)
- spacing = 3 (2^(4-2) - 1 = 2^2 - 1 = 4 - 1 = 3)

level 4:
- indent = 0: (2^(4-4) - 1 = 2^0 - 1 = 1 - 1 = 0)
- spacing = 1: (2^(4-3) - 1 = 2^1 - 1 = 2 - 1 = 1)

2^(4-4+1)-2
2^(4-4+2)-1

[      ]                        [14, 31]  ______________1
[2^(4-2+1)-2=6; 2^(4-2+2)-1=15] [ 6, 15]  ______2...............3...............
[2^(4-3+1)-2=2; 2^(4-3+2)-1= 7] [ 2,  7]  __4.......5.......6.......7.......
[2^(4-4+1)-2=0; 2^(4-4+2)-1= 3] [ 0,  3]  8...9...0...0...0...0...0...0...

[21, x]   _____________________1
[9, 23]   _________2.......................3.......................
[3, 11]   ___4...........5...........6...........7...........
[0,  5]   8.....9.....0.....0.....0.....0.....0.....0.....


irb(main):009:0> [3*(2**0)-3, 3*(2**1)-3, 3*(2**2)-3, 3*(2**3)-3]
=> [0, 3, 9, 21]

irb(main):044:0> [3*(2**1)-1, 3*(2**2)-1, 3*(2**3)-1, 3*(2**4)-1]
=> [5, 11, 23, 47]

5,11,23
6,12,24

[2,8,20]
[9, 21, x]

[21,  x]  _____________________1...............................................
[20,  x]  ____________________---
[ 9, 23]  _________2.......................3.......................
[ 8, 21]  ________---                     ---
[ 3, 11]  ___4...........5...........6...........7...........
[ 2,  9]  __---         ---         ---         ---
[ 0,  5]  8.....9.....0.....0.....0.....0.....0.....0.....


[21,  x]_____________________1...............................................
[20,  x]____________________/ \
[ 9, 23]_________2.......................3.......................
[ 8, 21]________/ \                     / \
[ 3, 11]___4...........5...........6...........7...........
[ 2,  9]__/ \         / \         / \         / \
[ 1,  7]_/   \       /   \       /   \       /   \
[ 0,  5]8.....9.....0.....0.....0.....0.....0.....0.....


For depth=4, I need pad_left=21    [3 * (2**3) - 3]
For depth=3, I need pad_left=9     [3 * (2**2) - 3]
For depth=2, I need pad_left=3     [3 * (2**1) - 3]
For depth=1, I need pad_left=0     [3 * (2**0) - 3]

For depth=4, I need 11 subrows    [3 * (2**2) - 1]
For depth=3, I need 5 subrows     [3 * (2**1) - 1]
For depth=2, I need 2 subrows     [3 * (2**0) - 1]
For depth=1, I need 0 subrows     [NOOP]

[0, 3,  9, 21]
[1, 4, 10, 22]
[2, 5, 11, 23]
[3, 6, 12, 24]


[      ][21             ] _____________________1...............................................
[      ][20             ] ____________________/ \
[      ][19             ] ___________________/   \
[      ][18             ] __________________/     \
[      ][17             ] _________________/       \
[      ][16             ] ________________/         \
[      ][15             ] _______________/           \
[      ][14             ] ______________/             \
[      ][13             ] _____________/               \
[      ][12             ] ____________/                 \
[11, 19][11, 19         ] ___________/                   \
[10, 21][10, 21         ] __________/                     \               11 subrows (2**2) + (2**3) - 1
[9, 23 ][9,23...        ] _________2.......................3.......................
[8, 22 ][8, 1, 21       ] ________/ \                     / \
[7, 22 ][7, 3, 19       ] _______/   \                   /   \
[6, 22 ][6, 5, 17       ] ______/     \                 /     \
[5, 22 ][5, 7, 15, 7    ] _____/       \               /       \
[4, 22 ][4, 9, 13, 9    ] ____/         \             /         \          5 subrows (2**1) + (2**2) - 1
[3, 11 ][3, 11, 11, ... ] ___4...........5...........6...........7...........
[2, 11 ][2  2, 9, 2, 9  ] __/ \........./ \         / \         / \
[1, 10 ][1, 3, 7, 3, 7  ] _/...\......./   \       /   \       /   \       2 subrows (2**0) + (2**1) - 1
[0, 10 ][0, 5, 5, ...   ] 8.....9.....0.....0.....0.....0.....0.....0.....


[1,4,10]
[2,5,11]
[3,6,12]

[1,3,7]
[2,4,8]




              1
              /\
      2               3
      /\               /\
     /  \             /  \
    /    \           /    \
   /      \         /      \
  4       5       6       7
  /\       /\       /\       /\
8   9   0   0   0   0   0   0
/\   /\   /\   /\   /\   /\   /\   /\


*)

let rec pow x n =
  match n with
  | 0 -> 1
  | _ -> x * pow x (n - 1)
;;

let test =
  let groups =
    [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6; 7 ]; [ 8; 9; 0; 0; 0; 0; 0; 0 ] ]
  in
  let test_group gi max_level group =
    let level = gi + 1 in
    (* let indent =  pow 2 (max_level - level + 1) - 2 + 0 in *)
    (* 3*(2**0)-3 *)
    let indent = (3 * pow 2 (max_level - level)) - 3 in
    (* let spacing = pow 2 (max_level - level + 2) - 1 + 0 in *)
    (* 3*(2**1)-1 *)
    let spacing = (3 * pow 2 (max_level - level + 1)) - 1 in
    let pad_left = Printf.sprintf "%s" (repeat indent "_") in
    let pad_left1 =
      if level = max_level then
        ""
      else
        Printf.sprintf "%s" (repeat (indent - 1) "_")
    in
    let pad_left2 =
      if level = max_level then
        ""
      else
        Printf.sprintf "%s" (repeat (indent - 2) "_")
    in
    let _ =
      Printf.printf "DEBUG:(level=%d/%d, indent=%d, spacing=%d)\n" level
        max_level indent spacing
    in
    let render x = Printf.sprintf "%d%s" x (repeat spacing ".") in
    let render2 (_x : int) =
      if level = max_level then
        ""
      else
        Printf.sprintf "%s%s" "/ \\" (repeat (spacing - 2) " ")
    in
    let render3 (_x : int) =
      if level = max_level then
        ""
      else
        Printf.sprintf "%s%s" "/   \\" (repeat (spacing - 4) " ")
    in

    let res =
      pad_left :: List.mapi (fun i item -> render item) group
      |> String.concat ""
    in
    let res2 =
      pad_left1 :: List.mapi (fun i item -> render2 item) group
      |> String.concat ""
    in
    let res3 =
      pad_left2 :: List.mapi (fun i item -> render3 item) group
      |> String.concat ""
    in
    String.concat "\n" [ res; res2; res3 ]
    (* String.concat "\n" [ res ] *)
  in
  groups
  |> List.mapi (fun i group -> test_group i (List.length groups) group)
  |> String.concat "\n"
;;

let () =
  let ten_items =
    Br ( 10
      , Br ( 5
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
        , Lf
        )
  in

  print_newline ();
  print_endline @@ string_of_tree ten_items;
  print_newline ();
  print_endline @@ test;
  print_endline @@ string_of_tree @@ Lib.Chapter11.seven_items;
  print_endline @@ string_of_tree @@ Lib.Chapter11.tree_of_list [ 4;2;6;1;3;5;7 ];
  [@@ocamlformat "disable"]
