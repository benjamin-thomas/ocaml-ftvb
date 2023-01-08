(*
  Lib.Chapter12.print_dict_entry (1, "one");;
*)
let print_dict_entry (k, v) =
  print_int k;
  print_newline ();
  print_string v;
  print_newline ()
;;

(*
   Lib.Chapter12.print_dict [(1, "one"); (2, "two")];;
*)
let rec print_dict d =
  match d with
  | [] -> ()
  | h :: t ->
      print_dict_entry h;
      print_dict t
;;

(*
   Lib.Chapter12.(iter print_dict_entry [(1, "one"); (2, "two")]);;
*)
let rec iter f lst =
  match lst with
  | [] -> ()
  | h :: t ->
      f h;
      iter f t
;;

(*
   Lib.Chapter12.print_dict' [(1, "one"); (2, "two")];;
   Lib.Chapter12.print_dict'' [(1, "one"); (2, "two")];;
*)
let print_dict' d = iter print_dict_entry d
let print_dict'' = iter print_dict_entry

(* Lib.Chapter12.read_dict () *)
let rec read_dict () =
  let k = read_int () in
  if k = 0 then
    []
  else
    let v = read_line () in
    (k, v) :: read_dict ()
;;

let rec read_dict' () =
  try
    let k = read_int () in
    if k = 0 then
      []
    else
      let v = read_line () in
      (k, v) :: read_dict' ()
  with
  | Failure _ ->
      print_string "This is not a valid integer. Please try again.";
      print_newline ();
      (* The function call is "canceled" in a way, but not the prior calls (on the left of the cons operator) *)
      read_dict' ()
;;

(*
  Places we can read from have type `in_channel`.
  Places we can write to have type `out_channel`.
*)
let entry_to_channel ch (k, v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'
;;

let dict_to_channel ch d = iter (entry_to_channel ch) d

(*
    How do we obtain a `channel`?
    The function `open_out` gives an output channel for a given filename.
    We must call `close_out` to properly close the file.

    `close_out` may raise an exception.

    Lib.Chapter12.dict_to_file_exn "/tmp/tmp" [(1, "one"); (2, "two"); (3, "three")];;
    Lib.Chapter12.(dict_to_file_exn "/tmp/tmp" @@ read_dict' ());;
*)
let dict_to_file_exn path d =
  let ch = open_out path in
  dict_to_channel ch d;
  close_out ch
;;

(*
  int_of_string can be thought as "int from string"
  entry_of_channel can be thought as "entry from channel"
*)
let entry_of_channel_exn ch =
  let k = input_line ch |> int_of_string in
  let v = input_line ch in
  (k, v)
;;

let rec dict_of_channel_exn ch =
  try
    let e = entry_of_channel_exn ch in
    e :: dict_of_channel_exn ch
  with
  | End_of_file -> []
;;

(*
  Lib.Chapter12.dict_of_file_exn "/tmp/tmp";;
*)

let dict_of_file_exn path =
  let ch = open_in path in
  let dict = dict_of_channel_exn ch in
  close_in ch;
  dict
;;

(* QUESTIONS *)

(*
  1. Write a function to print a list of integers to the screen in the same
     format  OCaml uses — i.e. with square brackets and semicolons.

     # [1;2;3;4];;
     - : int list = [1; 2; 3; 4]

     Lib.Chapter12.print_int_list [1;2;3;4];;
*)
let print_int_list lst =
  let rec inner lst =
    match lst with
    | [] -> ()
    | h :: t ->
        print_int h;
        if t <> [] then print_string "; ";
        inner t
  in
  print_newline ();
  print_string "---";
  print_newline ();
  print_string "- : int list = [";
  inner lst;
  print_string "]";
  print_newline ();
  print_string "---";
  print_newline ()
;;

(*
  2. Write a function to read three integers from the user, and return them as a tuple.
     What exceptions could be raised in the process? Handle them appropriately.

     Lib.Chapter12.read_three_ints ();;
*)

let read_three_ints () =
  let rec read_or_retry label =
    try
      print_string @@ "Enter " ^ label ^ " number: ";
      read_int ()
    with
    | Failure reason ->
        print_string @@ "ERROR! Could not read int! Failure reason: " ^ reason;
        print_newline ();
        read_or_retry label
  in
  let x = read_or_retry "first" in
  let y = read_or_retry "second" in
  let z = read_or_retry "third" in
  (x, y, z)
;;

let read_three_ints' () =
  let rec read_or_retry label =
    let result =
      print_string @@ "Enter " ^ label ^ " number: ";
      read_int_opt ()
    in
    match result with
    | None ->
        print_string @@ "That was not a number! Try again!";
        print_newline ();
        read_or_retry label
    | Some x -> x
  in
  let x = read_or_retry "first" in
  let y = read_or_retry "second" in
  let z = read_or_retry "third" in
  (x, y, z)
;;

(*
  3. In our `read_dict` function, we waited for the user to type "0" to indicate no more data.
     This is clumsy. Implement a new `read_dict` function with a nicer system. Be careful to
     deal with possible exceptions which may be raised
*)

let rec read_dict'' () =
  try
    let l = read_line () in
    if l = "" then
      []
    else
      let elems = String.split_on_char ' ' l in
      match elems with
      | [ k; v ] ->
          let k = int_of_string k in
          (k, v) :: read_dict'' ()
      | _ -> failwith "Bad input, try again!"
  with
  | Failure _ ->
      print_endline "ERROR, retry!";
      read_dict'' ()
;;

(*
  4. Write a function which, given a number `x`, prints the x-times table to a given file name.
     For example, `table "table.txt" 5` should produce a file `table.txt` containing the following:

1     2     3     4     5
2     4     6     8    10
3     6     9    12    15
4     8    12    16    20
5    10    15    20    25

*)

(** `line_for` generates the times table for `x`, starting at `x`, for `n` elements. *)
let line_for x n =
  let rec inner inc_by curr cnt_down =
    if cnt_down = 0 then
      []
    else
      curr :: inner inc_by (curr + inc_by) (cnt_down - 1)
  in
  assert (n > 0);
  inner x x n
;;

let%test_unit _ =
  [%test_eq: Base.int Base.list] [ 1; 2; 3; 4; 5 ] @@ line_for 1 5
;;

let%test_unit _ =
  [%test_eq: Base.int Base.list] [ 2; 4; 6; 8; 10 ] @@ line_for 2 5
;;

let%test_unit _ =
  [%test_eq: Base.int Base.list] [ 3; 6; 9; 12; 15 ] @@ line_for 3 5
;;

(** Maps `line_for` `x` times, for the `n` times table *)
let lines_for lines_cnt cols_cnt =
  let rec inner x n cnt_up =
    if cnt_up > x then
      []
    else
      line_for cnt_up n :: inner x n (cnt_up + 1)
  in
  inner lines_cnt cols_cnt 1
;;

let%test_unit _ =
  [%test_eq: Base.int Base.list Base.list]
    [ [ 1; 2; 3; 4; 5 ]; [ 2; 4; 6; 8; 10 ]; [ 3; 6; 9; 12; 15 ] ]
  @@ lines_for 3 5
;;

let elem_to_ch ch elem = output_string ch @@ string_of_int elem ^ "\t"

let line_to_ch ch line =
  iter (elem_to_ch ch) line;
  output_string ch "\n"
;;

let lines_to_ch ch lines = iter (line_to_ch ch) lines

(*
  Lib.Chapter12.table "/tmp/table.txt" 5;;
*)
let table path x =
  let lines = lines_for x x in
  let ch = open_out path in
  lines_to_ch ch lines;
  close_out ch
;;

(*
  5. Write a function to count the number of lines in a given file

  Lib.Chapter12.lines_cnt "/tmp/table.txt";;
*)
let lines_cnt path =
  let ch = open_in path in
  let rec inner x =
    try
      input_line ch |> ignore;
      inner x + 1
    with
    | End_of_file -> x
  in
  inner 0
;;

(*
  6. Write a function `copy_file` of type `string -> string -> unit` which copies a file line by line.
     For example, `copy_file "a.txt" "b.txt"` should produce a file `b.txt` identical to `a.txt`.
     Make sure you deal with the case where the file `a.txt` cannot be found, or where `b.txt` cannot
     be created or filled.
*)

let rec copy_lines i o =
  try
    let line = input_line i in
    output_string o @@ line ^ "\n";
    copy_lines i o
  with
  | End_of_file -> ()
;;

exception Copy_error of string

let copy_file a b =
  try
    let i = open_in a in
    let o = open_out b in
    copy_lines i o;
    close_out o;
    close_in i
  with
  | Sys_error err -> raise @@ Copy_error err
  | _ -> raise @@ Copy_error "Unknow error!"
;;
