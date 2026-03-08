(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib
open Frontend
open Middleend
open Parser
open Utils.Pretty_printer

let run str =
  match parse str with
  | Error error -> Format.printf "%s" error
  | Ok ast ->
    (match Ll.lambda_lifting_result ast with
     | Error e -> Format.eprintf "%a\n" Ll.pp_error e
     | Ok lst -> Format.printf "%a\n" pp_structure lst)
;;

let%expect_test "simple ll" =
  run
    {|
  let foo a =
    let fn = (fun a b -> a + b) a in
    fn 3
  ;;
  |};
  [%expect {|
    let lifted_0 = fun a b -> (a + b);;
    let foo = fun a -> let fn = lifted_0 a in fn 3;; |}]
;;

let%expect_test "let in ll" =
  run
    {|
  let test1 x y = let test2 x y z = x, y, z in test2 x y;;
  |};
  [%expect {|
    let lifted_0 = fun x y z -> (x, y, z);;
    let test1 = fun x y -> let test2 = lifted_0 in test2 x y;; |}]
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
  [%expect {|
    let lifted_2 = fun k n m -> k (* m n);;
    let lifted_1 = fun n k -> if <= n 1 then k 1 else lifted_0 (- n 1) (lifted_2 k n);;
    let rec lifted_0 = lifted_1;;
    let lifted_3 = fun x -> x;;
    let fac = fun n -> lifted_0 n lifted_3;; |}]
;;

let%expect_test "nested ll" =
  run
    {|
  let outer x =
    let mid x y = let inner x y z = ( + ) (( + ) x y) z in inner x y 3 in
    mid x 4
  ;;
  |};
  [%expect {|
    let lifted_1 = fun x y z -> + (+ x y) z;;
    let lifted_0 = fun x y -> let inner = lifted_1 in inner x y 3;;
    let outer = fun x -> let mid = lifted_0 in mid x 4;; |}]
;;

let%expect_test "if then else with ll" =
  run
    {|
  let foo flag a b = if flag then (fun a x -> a + x) a else (fun b x -> b + x) b
  |};
  [%expect {|
    let lifted_0 = fun a x -> (a + x);;
    let lifted_1 = fun b x -> (b + x);;
    let foo = fun flag a b -> if flag then lifted_0 a else lifted_1 b;; |}]
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
  [%expect {|
    let lifted_1 = function | 1 -> 1 | a -> lifted_0 ((a - 1));;
    let rec lifted_0 = lifted_1;;
    let foo = function | 0 -> 0 | _ -> lifted_0 3;; |}]
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
  [%expect {|
    let lifted_0 = fun y z -> (y + z);;
    let lifted_1 = fun z -> z;;
    let f = fun x -> match x with | Some (y) -> lifted_0 y | None -> lifted_1;; |}]
;;
