let make_vector (x0, y0) (x1, y1) = (x1 -. x0, y1 -. y0)
let vector_len (x, y) = sqrt ((x *. x) +. (y *. y))
let offset_point (x, y) (px, py) = (px +. x, py +. y)

let scale_to_len l (a, b) =
  let curr_len = vector_len (a, b) in
  if curr_len = 0.0 then
    (a, b)
  else
    let factor = l /. curr_len in
    (a *. factor, b *. factor)
;;

(* QUESTIONS *)

(*
  1. Give a function which rounds a positive floating-point number to the
     nearest whole number, returning another floating-point number.

     Float.round
*)
let round x =
  let c = ceil x in
  let f = floor x in
  let () =
    Printf.printf "c=%f, f=%f\t\tc-x=%f\t\tx-f=%f\n" c f (c -. x) (x -. f)
  in
  if c -. x <= x -. f then
    c
  else
    f
;;

(*
  2. Write a function to find the point equidistant from two given points in two dimensions
*)

let equidistant_point (x1, y1) (x2, y2) =
  let x = (x1 +. x2) /. 2.0 in
  let y = (y1 +. y2) /. 2.0 in
  (x, y)
;;

(*
  3. Write a function to separate a floating-point into its whole and fractional parts.
     Return them as a tuple of type `float * float`
*)

let sep n =
  let fract, whole = modf n in
  (whole, fract)
;;

let rec parts x =
  if x < 0.0 then
    let a, b = parts (-.x) in
    (-.a, b)
  else
    (floor x, x -. floor x)
;;

let parts' x =
  if x < 0.0 then
    let cx = ceil x in
    (cx, cx -. x)
  else
    let fx = floor x in
    (fx, x -. fx)
;;

(* let%test_unit _ = [%test_eq: Base.float * Base.float] (3.0, 0.14) @@ parts' 3.14 *)

type expect = { want : float * float; got : float * float }

let expect_ff_tuples =
  let open Base in
  List.map ~f:(fun exp -> [%test_eq: float * float] exp.want exp.got)
;;

let (_ : unit list) =
  expect_ff_tuples
    [ { want = (2.0, 0.5); got = parts 2.5 }
    ; { want = (2.0, 0.5); got = parts' 2.5 }
    ; { want = (-3.0, 0.5); got = parts (-3.5) }
    ; { want = (-3.0, 0.5); got = parts' (-3.5) }
    ]
;;

let%test_unit _ = [%test_eq: Base.float * Base.float] (2.0, 0.5) @@ parts 2.5

let%test_unit _ =
  [%test_eq: Base.float * Base.float] (-2.0, 0.5) @@ parts (-2.5)
;;

(*
  4. Write a function `star` of type `float -> unit` which, given a floating-point
     number between zero and one, draws an asterisk to indicate the position.

     An argument of zero will result in asterisk in column one, and an argument of one
     an an asterisk in column fifty.
*)

let star x =
  let i = int_of_float (floor (x *. 50.0)) in
  for _ = 1 to i - 1 do
    print_char ' '
  done
  ; print_char '*'
  ; print_newline ()
;;

(*
  let pi = 4.0 *. atan 1.0;;
  Lib.Chapter14.plot sin 0.0 pi (pi /. 20.0);;
*)
let plot f a b dy =
  let pos = ref a in
  while !pos <= b do
    star (f !pos)
    ; pos := !pos +. dy
  done
;;

let%expect_test _ =
  let pi = 4.0 *. atan 1.0 in
  let () = plot sin 0.0 pi (pi /. 20.0) in
  [%expect
    {|
    *
          *
                  *
                         *
                                *
                                      *
                                           *
                                               *
                                                  *
                                                    *
                                                     *
                                                    *
                                                  *
                                               *
                                           *
                                      *
                                *
                         *
                  *
          *
    * |}]
;;
