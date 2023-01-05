[@@@warning "-32-26"]

(*
   dune exec --display=quiet bin/print_tree_horiz.exe

   See also: https://www.leetcode-tree-visualizer.com/
     8,4,12,2,6,10,14,1,3,5,7,9,11,13,15

   And: https://visualgo.net/en/bst?slide=1

*)

module Tree = struct
  include Lib.Chapter11

  (* type 'a tree = 'a Lib.Chapter11.tree <-- won't have access to the `Br` and `Lf` variants that way *)
  type 'a tree = 'a Lib.Chapter11.tree = Br of 'a * 'a tree * 'a tree | Lf
end

open Tree

let to_str (n, str) = Printf.sprintf "(%d, %s)" n str

(*

  Pretty-print a tree horizontally, using box-drawing symbols.

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

---

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
let string_of_tree (string_of : 'a -> string) (tr : 'a tree) =
  let buffer = Buffer.create 1024 in

  let rec inner tr prefix is_right =
    let pr, pm, pl =
      if is_right then
        ("    ", "┌── ", "│   ")
      else
        ("│   ", "└── ", "    ")
    in

    let render_line x buffer =
      let line = prefix ^ pm ^ string_of x in
      (* Printf.printf "DEBUG:%s (%s,%s)\n" line prefix pm |> ignore; *)
      Buffer.add_string buffer (line ^ "\n")
    in

    match tr with
    | Lf -> ()
    | Br (x, l, r) ->
        inner r (prefix ^ pr) true;
        buffer |> render_line x;
        inner l (prefix ^ pl) false
  in

  match tr with
  | Lf -> failwith "Invalid arg!"
  | Br (x, l, r) ->
      inner r "" true;
      Buffer.add_string buffer (string_of x ^ "\n");
      inner l "" false;
      Buffer.contents buffer
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
  (* print_endline @@ string_of_tree string_of_int ten_items ;
  print_endline @@ string_of_tree string_of_int Lib.Chapter11.seven_items; *)
  print_endline @@ string_of_tree string_of_int @@ Lib.Chapter11.tree_of_list [ 4; 2; 6; 1; 3; 5; 7 ];
  print_endline @@ string_of_tree string_of_int @@ Lib.Chapter11.tree_of_list [ 4; 2; 1; 3; 6; 5; 7 ];
  print_endline @@ string_of_tree to_str @@ Lib.Chapter11.tree_of_list' [ (4, "four"); (2, "two"); (6, "six"); (1, "one"); (3, "three"); (5, "five"); (7, "seven") ];
  print_endline @@ string_of_tree to_str @@ Lib.Chapter11.tree_of_list' [ (4, "four"); (2, "two"); (1, "one"); (3, "three"); (6, "six"); (5, "five"); (7, "seven") ];
  print_endline @@ string_of_tree string_of_int @@ Lib.Chapter11.tree_of_list'' [ 4; 2; 6; 1; 3; 5; 7 ];
  (* print_endline @@ string_of_tree to_str Lib.Chapter11.balanced_tree;
  print_endline @@ string_of_tree string_of_int Lib.Chapter11.nine_items;
  print_endline @@ string_of_tree string_of_int Lib.Chapter11.nine_items_flipped;
  print_endline @@ string_of_tree string_of_int @@ Lib.Chapter11.flip Lib.Chapter11.nine_items; *)
  (* print_endline @@ string_of_tree string_of_int @@ Lib.Chapter11.tree_of_list [ 5; 4; 6; ]; *)
  (* print_endline @@ string_of_tree string_of_int @@ Lib.Chapter11.tree_of_list [ 5; 4; 6; 2; 8; 1; 3; 7; 9 ]; *)
  [@@ocamlformat "disable"]
