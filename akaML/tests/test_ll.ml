[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser
open Pprinter
open Middleend

let run str =
  match parse str with
  | Ok ast -> Format.printf "%a \n" pp_structure (Lambda_lifting.lambda_lifting ast)
  | Error error -> Format.printf "%s" error
;;

let%expect_test "simple ll" =
  run
    {|
  let foo a =
    let fn = (fun a b -> a + b) a in
    fn 3
  ;;
  |};
  [%expect
    {|
  let ll_0 a b = ( + ) a b;;
  let foo a = let fn = ll_0 a in fn 3;;
  |}]
;;

let%expect_test "let in ll" =
  run
    {|
  let test1 x y = let test2 x y z = x, y, z in test2 x y;;
  |};
  [%expect
    {|
  let ll_0 x y z = x, y, z;;
  let test1 x y = let test2 = ll_0 in test2 x y;;
  |}]
;;

let%expect_test "fac ll" =
  run
    {|
  let fac n =
    let rec fack n k =
    if ( <= ) n 1 then k 1
    else fack (( - ) n 1) ((fun k n m -> k (( * ) m n)) k n) in
    fack n (fun x -> x)
  ;;
  |};
  [%expect
    {|
  let ll_1 k n m = k (( * ) m n);;
  let rec ll_0 n k = if ( <= ) n 1 then k 1 else ll_0 (( - ) n 1) (ll_1 k n);;
  let ll_2 x = x;;
  let fac n = ll_0 n ll_2;;
  |}]
;;

let%expect_test "nonrecursive multiple lets 1" =
  run
    {|
  let foo x = let bar x y = ( + ) x y
              and baz = 2 in ( + ) (bar x 2) baz;;
  |};
  [%expect
    {|
  let ll_0 x y = ( + ) x y;;
  let foo x = let bar = ll_0
              and baz = 2 in ( + ) (bar x 2) baz;;
  |}]
;;

let%expect_test "nonrecursive multiple lets 2" =
  run
    {|
  let foo x = let bar y = y
              and baz x c = ( + ) x c in ( + ) (bar 2) (baz x 5);;
  ;;
  |};
  [%expect
    {|
  let ll_0 y = y;;
  let ll_1 x c = ( + ) x c;;
  let foo x = let bar = ll_0
              and baz = ll_1 in ( + ) (bar 2) (baz x 5);;
  |}]
;;

let%expect_test "recursive multiple lets 1" =
  run
    {|
  let foo =
    let count = 10 in
    (let rec is_small count n =
     if ( <= ) n count then true else is_big count (( - ) n 1)
     and is_big count n =
     if ( > ) n count then false else is_small count (( - ) n 1) in
     is_small count 13)
  ;;
  |};
  [%expect
    {|
  let rec ll_0 count n =
    if ( <= ) n count then true else ll_1 count (( - ) n 1)
  and ll_1 count n = if ( > ) n count then false else ll_0 count (( - ) n 1);;
  let foo =
    let count = 10 in ll_0 count 13;;
  |}]
;;

let%expect_test "recursive multiple lets 2" =
  run
    {|
  let foo x =
    let rec bar x y = ( + ) x y
    and baz x c = ( + ) c (bar x 5) in
    ( + ) (bar x 5) (baz x 6)
  ;;
  |};
  [%expect
    {|
  let rec ll_0 x y = ( + ) x y
  and ll_1 x c = ( + ) c (ll_0 x 5);;
  let foo x = ( + ) (ll_0 x 5) (ll_1 x 6);;
  |}]
;;

let%expect_test "nested ll" =
  run
    {|
  let outer x =
    let mid x y = let inner x y z = ( + ) (( + ) x y) z in inner x y 3 in
    mid x 4
  ;;
  |};
  [%expect
    {|
  let ll_1 x y z = ( + ) (( + ) x y) z;;
  let ll_0 x y = let inner = ll_1 in inner x y 3;;
  let outer x = let mid = ll_0 in mid x 4;;
  |}]
;;

let%expect_test "if then else with ll" =
  run
    {|
  let foo flag a b = if flag then (fun a x -> a + x) a else (fun b x -> b + x) b
  |};
  [%expect
    {|
  let ll_0 a x = ( + ) a x;;
  let ll_1 b x = ( + ) b x;;
  let foo flag a b = if flag then ll_0 a else ll_1 b;;
  |}]
;;

let%expect_test "function ll" =
  run
    {|
  let foo = function
    | 0 -> 0
    | _ ->
      let rec fn = function
        | 1 -> 1
        | a -> fn (a - 1)
      in
      fn 3
  ;;
  |};
  [%expect
    {|
  let rec ll_0 = function
                 | 1 -> 1
                 | a -> ll_0 (( - ) a 1);;
  let foo = function
            | 0 -> 0
            | _ -> ll_0 3;;
  |}]
;;

let%expect_test "match exp ll" =
  run
    {|
  let f x =
    match x with
    | Some y -> (fun y z -> y + z) y
    | None -> fun z -> z
  ;;
  |};
  [%expect
    {|
  let ll_0 y z = ( + ) y z;;
  let ll_1 z = z;;
  let f x = match x with
            | Some (y) -> ll_0 y
            | None -> ll_1;;
  |}]
;;

let%expect_test "sequence with ll" =
  run
    {|
  let g x = print_int x; (let h x y = ( + ) x y in h x 10);;
  |};
  [%expect
    {|
  let ll_0 x y = ( + ) x y;;
  let g x = print_int x; (let h = ll_0 in h x 10);;
  |}]
;;

let%expect_test "tuple ll" =
  run
    {|
  let pair_sum a b =
    let f a b ( x, y ) = ( + ) (( + ) (( + ) a b) x) y in f a b ( 1, 2 )
  ;;
  |};
  [%expect
    {|
  let ll_0 a b ( x, y ) = ( + ) (( + ) (( + ) a b) x) y;;
  let pair_sum a b = let f = ll_0 in f a b ( 1, 2 );;
  |}]
;;
