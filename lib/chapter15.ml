(* About the standard library *)

(* QUESTIONS *)

(*
  1. Write your own version of the function `List.concat`.

     The implementation OCaml provides is not tail-recursive.
     Can you write on which is?
*)

type expect = { want : int list; got : int list }

let expect_int_lists =
  let open Base in
  List.map ~f:(fun exp -> [%test_eq: int list] exp.want exp.got)
;;

let concat lst =
  let rec loop lst acc =
    match lst with
    | [] -> acc
    | h :: t -> loop t (acc @ h)
  in
  loop lst []
;;

let (_ : unit list) =
  expect_int_lists
    [ { want = [ 1; 2; 3; 4; 5 ]; got = List.concat [ [ 1; 2; 3 ]; [ 4; 5 ] ] }
    ; { want = [ 1; 2; 3; 4; 5 ]; got = concat [ [ 1; 2; 3 ]; [ 4; 5 ] ] }
    ]
;;

(*
  2. Use `List.mem` to write a function which returns `true` only if every list in a
     `bool list list` contains `true` somewhere in it.
*)

let rec all_true = function
  | [] -> true
  | h :: t ->
      (* let () = Printf.printf "hd:%b\n" h in *)
      h && all_true t
;;

let%test _ = not @@ all_true [ false ]
let%test _ = all_true [ true ]
let%test _ = all_true [ true; true ]
let%test _ = not @@ all_true [ true; false; true ]

let rec any_true = function
  | [] -> false
  | h :: t -> h || any_true t
;;

let%test _ = not @@ any_true [ false ]
let%test _ = any_true [ true ]
let%test _ = any_true [ true; true ]
let%test _ = any_true [ false; false; true ]

let rec any_true_in lst =
  match lst with
  | [] -> true
  | h :: t -> any_true h && any_true_in t
;;

let%test _ = any_true_in [ [ true ] ]
let%test _ = not @@ any_true_in [ [ false ] ]
let%test _ = any_true_in [ [ true ]; [ true ] ]
let%test _ = not @@ any_true_in [ [ false ]; [ true ] ]
let%test _ = any_true_in [ [ true; false ]; [ false; true ] ]
let%test _ = not @@ any_true_in [ [ true; false ]; [ false; false ] ]

(* Oops, didn't read the question properly. Let's redo this. *)

let rec any_true_in' lst =
  match lst with
  | [] -> true
  | h :: t -> List.mem true h && any_true_in' t
;;

let%test _ = any_true_in' [ [ true ] ]
let%test _ = not @@ any_true_in' [ [ false ] ]
let%test _ = any_true_in' [ [ true ]; [ true ] ]
let%test _ = not @@ any_true_in' [ [ false ]; [ true ] ]
let%test _ = any_true_in' [ [ true; false ]; [ false; true ] ]
let%test _ = not @@ any_true_in' [ [ true; false ]; [ false; false ] ]

(* Solution from the book: "all_contain_true" *)
let all_contain_true lst = not (List.mem false (List.map (List.mem true) lst))

let%test _ = all_contain_true [ [ true ] ]
let%test _ = not @@ all_contain_true [ [ false ] ]
let%test _ = all_contain_true [ [ true ]; [ true ] ]
let%test _ = not @@ all_contain_true [ [ false ]; [ true ] ]
let%test _ = all_contain_true [ [ true; false ]; [ false; true ] ]
let%test _ = not @@ all_contain_true [ [ true; false ]; [ false; false ] ]

(*
  3. Write a function to count the number of exclamation marks in a string, using
     one or more functions from the `String` module
*)

let cnt c =
  0
  |> String.fold_left @@ fun total c' ->
     if c' = c then
       total + 1
     else
       total
;;

let%test _ = 0 = cnt '!' "hello"
let%test _ = 1 = cnt '!' "hello!"
let%test _ = 2 = cnt '!' "hello!!"
let%test _ = 6 = cnt '!' "h!e!l!lo!!!"

(*
  4. Use the `String.map` function to write a function to return a new copy of a string
     with all exclamation marks replaced with periods (full stops).
*)

let replace a b =
  String.map (fun c ->
      if c = a then
        b
      else
        c)
;;

let%test _ = "hello" = replace '!' '.' "hello"
let%test _ = "hello." = replace '!' '.' "hello!"
let%test _ = "hello.." = replace '!' '.' "hello!!"
let%test _ = "h.e.l.lo..." = replace '!' '.' "h!e!l!lo!!!"

(*
  5. Use the `String` module to write a function which concatenates a list of strings together.
*)

let join lst = String.concat " " lst

let%test _ = "hello world" = join [ "hello"; "world" ]

(*
  6. Do the same with the `Buffer` module: this will be faster.
*)

let join' lst =
  let separator tail =
    if tail = [] then
      ""
    else
      " "
  in
  let rec inner lst buf =
    match lst with
    | [] -> buf
    | h :: t ->
        let () = Buffer.add_string buf (h ^ separator t) in
        inner t buf
  in

  inner lst (Buffer.create 16) |> Buffer.contents
;;

let%test_unit _ =
  [%test_eq: Base.string] "hello world" @@ join' [ "hello"; "world" ]
;;

(* Book solution *)
let join' lst =
  let buf = Buffer.create 100 in
  List.iter (Buffer.add_string buf) lst
  ; Buffer.contents buf
;;

let%test_unit _ =
  [%test_eq: Base.string] "helloworld" @@ join' [ "hello"; "world" ]
;;

(*
  7. Use the `String` module to count the number of occurrences of the string "OCaml"
     within a given string.
*)

(*
  NOTE: the book uses an imperative solution and warns that using a char list is slow.
*)
let occur str =
  let to_lst s = s |> String.to_seq |> List.of_seq in
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | 'O' :: 'C' :: 'a' :: 'm' :: 'l' :: t -> inner t (acc + 1)
    | _ :: t -> inner t acc
  in
  inner (to_lst str) 0
;;

(* let%test_unit _ = [%test_eq: Base.int] 0 @@ occur "OCaml" "Hello" *)
let%test_unit _ = [%test_eq: Base.int] 0 @@ occur "Hello"
let%test_unit _ = [%test_eq: Base.int] 1 @@ occur "HelloOCamlWorld"
let%test_unit _ = [%test_eq: Base.int] 2 @@ occur "HelloOCamlWorldOCaml"
