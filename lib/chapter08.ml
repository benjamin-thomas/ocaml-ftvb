let fst (x, _) = x
let snd (_, y) = y
let census = [ (1, 4); (2, 2); (3, 2); (4, 3); (5, 1); (6, 2) ]

let rec lookup id lst =
  match lst with
  | [] -> raise Not_found
  | (k, v) :: xs ->
      if k = id then
        v
      else
        lookup id xs

let%test_unit _ = [%test_eq: Base.int] 1 @@ lookup 5 census
let%test_unit _ = [%test_eq: Base.int] 3 @@ lookup 4 census

let%expect_test _ =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      lookup 99 census);
  [%expect {| Not_found |}]

(* My original attempt, convoluted *)
let add k v dict =
  let rec inner k v dict did_update =
    match dict with
    | [] ->
        if did_update then
          []
        else
          (k, v) :: []
    | (k', v') :: t ->
        if k = k' then
          (k, v) :: inner k v t true
        else
          (k', v') :: inner k v t did_update
  in
  inner k v dict false

(* From the book *)
let add' k v dict =
  match dict with
  | [] -> [ (k, v) ]
  | (k', v') :: t ->
      if k = k' then
        (k, v) :: t
      else
        (k', v') :: add k v t

let%test_unit "add new entry from empty" =
  [%test_eq: (Base.int * Base.int) Base.list] [ (2, 99) ] @@ add 2 99 []

let%test_unit "add new entry from empty" =
  [%test_eq: (Base.int * Base.int) Base.list] [ (2, 99) ] @@ add' 2 99 []

let%test_unit "add new entry from non-empty" =
  [%test_eq: (Base.int * Base.int) Base.list] [ (1, 1); (2, 2); (3, 99) ]
  @@ add 3 99 [ (1, 1); (2, 2) ]

let%test_unit "add new entry from non-empty" =
  [%test_eq: (Base.int * Base.int) Base.list] [ (1, 1); (2, 2); (3, 99) ]
  @@ add' 3 99 [ (1, 1); (2, 2) ]

let%test_unit "update existing entry" =
  [%test_eq: (Base.int * Base.int) Base.list] [ (1, 1); (2, 99); (3, 3) ]
  @@ add 2 99 [ (1, 1); (2, 2); (3, 3) ]

let%test_unit "update existing entry" =
  [%test_eq: (Base.int * Base.int) Base.list] [ (1, 1); (2, 99); (3, 3) ]
  @@ add' 2 99 [ (1, 1); (2, 2); (3, 3) ]

let rec remove k dict =
  match dict with
  | [] -> []
  | (k', v) :: t ->
      if k = k' then
        t
      else
        (k', v) :: remove k t

let%test_unit _ =
  [%test_eq: (Base.int * Base.int) Base.list] [ (1, 1); (3, 3) ]
  @@ remove 2 [ (1, 1); (2, 2); (3, 3) ]

let%test_unit _ =
  [%test_eq: (Base.int * Base.int) Base.list] [ (1, 1); (2, 2); (3, 3) ]
  @@ remove 99 [ (1, 1); (2, 2); (3, 3) ]

let key_exists k dict =
  try
    let _ = lookup k dict in
    true
  with
  | Not_found -> false

let%test_unit _ = [%test_eq: Base.int] 1 @@ lookup 1 [ (1, 1); (2, 2); (3, 3) ]

[@@@ocamlformat "disable"]

let%test _ =        key_exists 1 [ (1, 1); (2, 2); (3, 3) ]
let%test _ =        key_exists 2 [ (1, 1); (2, 2); (3, 3) ]
let%test _ =        key_exists 3 [ (1, 1); (2, 2); (3, 3) ]
let%test _ = not @@ key_exists 4 [ (1, 1); (2, 2); (3, 3) ]

[@@@ocamlformat "enable"]

(* Questions *)

(* 1. Write a function to determine the number of different (unique) keys in a dictionary *)

let uniq_keys dict =
  let rec inner dict uniq =
    match dict with
    | [] -> List.length uniq
    | (k, v) :: t ->
        if key_exists k uniq then
          inner t uniq
        else
          inner t ((k, v) :: uniq)
  in
  inner dict []

let%test_unit _ = [%test_eq: Base.int] 0 @@ uniq_keys []
let%test_unit _ = [%test_eq: Base.int] 1 @@ uniq_keys [ (1, 1) ]
let%test_unit _ = [%test_eq: Base.int] 2 @@ uniq_keys [ (1, 1); (2, 2) ]
let%test_unit _ = [%test_eq: Base.int] 2 @@ uniq_keys [ (1, 1); (2, 2); (1, 3) ]

(* 2. Define a function `replace` which is like `add`, but raises `Not_found` if
   the key is not already there *)

let rec replace k v dict =
  match dict with
  | [] -> raise Not_found
  | (k', v') :: t ->
      if k = k' then
        (k, v) :: t
      else
        (k', v') :: replace k v t

let%expect_test "raise on empty" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      replace 2 99 []);
  [%expect {| Not_found |}]

let%expect_test "raise if entry does not exist" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      replace 2 99 [ (1, 1); (3, 3); (4, 4) ]);
  [%expect {| Not_found |}]

let%test_unit "replace an existing entry" =
  [%test_eq: (Base.int * Base.int) Base.list] [ (1, 1); (2, 99); (3, 3) ]
  @@ replace 2 99 [ (1, 1); (2, 2); (3, 3) ]

(* 3. Write a function to build a dictionary from two equal length lists, one
   containing keys and another containing values. Raise the exception
   `Invalid_argument` if the lists are not of equal length. *)

let rec merge ls_a ls_b =
  if List.length ls_a <> List.length ls_b then
    raise @@ Invalid_argument "Lists length differ"
  else
    match (ls_a, ls_b) with
    | x :: xs, y :: ys -> (x, y) :: merge xs ys
    | _, _ -> []

let%test_unit _ = [%test_eq: (Base.int * Base.char) Base.list] [] @@ merge [] []

let%test_unit _ =
  [%test_eq: (Base.int * Base.char) Base.list] [ (1, 'A'); (2, 'B') ]
  @@ merge [ 1; 2 ] [ 'A'; 'B' ]

let%expect_test "fails if lengths differ" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () -> merge [] [ 1 ]);
  [%expect {| (Invalid_argument "Lists length differ") |}]

(* 4. Now write the inverse function: given a dictionary, return the pair of two
   lists - the first containing all the keys, and the second containing all the
   values *)

let unmerge dict =
  let rec inner dict lst_a lst_b =
    match dict with
    | [] -> (List.rev lst_a, List.rev lst_b)
    | (a, b) :: t -> inner t (a :: lst_a) (b :: lst_b)
  in
  inner dict [] []

let%test_unit _ =
  [%test_eq: Base.int Base.list * Base.char Base.list] ([], []) @@ unmerge []

let%test_unit _ =
  [%test_eq: Base.int Base.list * Base.char Base.list]
    ([ 1; 2; 3 ], [ 'A'; 'B'; 'C' ])
  @@ unmerge [ (1, 'A'); (2, 'B'); (3, 'C') ]

(* 5. Define a function to turn any list of pairs into a dictionary. If
   duplicate keys are found, the value associated with the first occurence of the
   key should be kept *)

let uniq_dict dict =
  let rec value_exists v dict =
    match dict with
    | [] -> false
    | v' :: t ->
        if v = v' then
          true
        else
          value_exists v t
  in
  let rec inner dict uniq =
    match dict with
    | [] -> []
    | (k, v) :: t ->
        if value_exists v uniq then
          inner t uniq
        else
          (k, v) :: inner t (v :: uniq)
  in
  inner dict []

let%test_unit _ =
  [%test_eq: (Base.int * Base.char) Base.list] [] @@ uniq_dict []

let%test_unit _ =
  [%test_eq: (Base.int * Base.char) Base.list] [ (1, 'A') ]
  @@ uniq_dict [ (1, 'A') ]

let%test_unit _ =
  [%test_eq: (Base.int * Base.char) Base.list] [ (1, 'A'); (2, 'B') ]
  @@ uniq_dict [ (1, 'A'); (2, 'B'); (3, 'A') ]

(* 6. Write the function `union a b` which forms the union of two dictionaries.
   The union of two dictionaries is the dictionary containing all the entries in
   one or other or both. In the case that a key is contained in both dictionaries,
   the value in the first should be preferred. *)

let union a b =
  let rec key_exists k dict =
    match dict with
    | [] -> false
    | k' :: t ->
        if k = k' then
          true
        else
          key_exists k t
  in
  let rec inner a b seen =
    match a with
    | [] ->
        if b = [] then
          []
        else
          inner b [] seen
    | (k, v) :: t ->
        if key_exists k seen then
          inner t b seen
        else
          (k, v) :: inner t b (k :: seen)
  in
  inner a b []

let%test_unit _ = [%test_eq: (Base.int * Base.char) Base.list] [] @@ union [] []

let%test_unit _ =
  [%test_eq: (Base.int * Base.char) Base.list] [ (1, 'A') ]
  @@ union [ (1, 'A') ] []

let%test_unit _ =
  [%test_eq: (Base.int * Base.char) Base.list] [ (1, 'A') ]
  @@ union [] [ (1, 'A') ]

let%test_unit _ =
  [%test_eq: (Base.int * Base.char) Base.list]
    [ (1, 'A'); (2, 'B'); (3, 'A'); (4, 'X') ]
  @@ union [ (1, 'A'); (2, 'B'); (3, 'A') ] [ (1, 'Z'); (4, 'X') ]
