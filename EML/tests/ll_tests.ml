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

let%expect_test "nonrecursive_multiple_lets" =
  run
    {|
  let foo x =
    let bar x y = x + y
    and baz = 2 in
    bar x 2 + baz
  ;;
  |};
  [%expect
    {|
    let lifted_0 = fun x y -> (x + y);;
    let foo = fun x -> let bar = lifted_0
    and baz = 2 in (bar x 2 + baz);; |}]
;;

let%expect_test "nonrecursive_multiple_functions" =
  run
    {|
  let foo x =
    let bar y = y
    and baz x c = x + c in
    bar 2 + baz x 5
  ;;
  |};
  [%expect
    {|
    let lifted_0 = fun y -> y;;
    let lifted_1 = fun x c -> (x + c);;
    let foo = fun x -> let bar = lifted_0
    and baz = lifted_1 in (bar 2 + baz x 5);; |}]
;;

let%expect_test "mutual_recursion_in_let_rec_and" =
  run
    {|
  let foo =
    let limit = 10 in
    let rec is_small limit n =
      if n <= limit then true else is_big limit (n - 1)
    and is_big limit n =
      if n > limit then false else is_small limit (n - 1) in
    is_small limit 13
  ;;
  |};
  [%expect
    {|
    let lifted_2 = fun limit n -> if (n <= limit) then true else lifted_1 limit ((n - 1));;
    let lifted_3 = fun limit n -> if (n > limit) then false else lifted_0 limit ((n - 1));;
    let rec lifted_0 = lifted_2
    and lifted_1 = lifted_3;;
    let foo = let limit = 10 in lifted_0 limit 13;; |}]
;;

let%expect_test "recursive_local_bindings_use_renamed_functions" =
  run
    {|
  let foo x =
    let rec bar x y = x + y
    and baz x c = c + bar x 5 in
    bar x 5 + baz x 6
  ;;
  |};
  [%expect
    {|
    let lifted_2 = fun x y -> (x + y);;
    let lifted_3 = fun x c -> (c + lifted_0 x 5);;
    let rec lifted_0 = lifted_2
    and lifted_1 = lifted_3;;
    let foo = fun x -> (lifted_0 x 5 + lifted_1 x 6);; |}]
;;

let%expect_test "sequence_with_local_lambda" =
  run
    {|
  let g x =
    print_int x;
    (let h x y = x + y in h x 10)
  ;;
  |};
  [%expect
    {|
    let lifted_0 = fun x y -> (x + y);;
    let g = fun x -> let () = print_int x in let h = lifted_0 in h x 10;; |}]
;;

let%expect_test "tuple_pattern_lambda_lifting" =
  run
    {|
  let pair_sum a b =
    let f a b (x, y) = a + b + x + y in
    f a b (1, 2)
  ;;
  |};
  [%expect
    {|
    let lifted_0 = fun a b (x, y) -> (((a + b) + x) + y);;
    let pair_sum = fun a b -> let f = lifted_0 in f a b ((1, 2));; |}]
;;

let%expect_test "match_with_option_and_inline_lambdas" =
  run
    {|
  let f x =
    match x with
    | Some y -> (fun y z -> y - z) y
    | None -> fun z -> z + 1
  ;;
  |};
  [%expect
    {|
    let lifted_0 = fun y z -> (y - z);;
    let lifted_1 = fun z -> (z + 1);;
    let f = fun x -> match x with | Some (y) -> lifted_0 y | None -> lifted_1;; |}]
;;
