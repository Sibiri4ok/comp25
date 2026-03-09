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
    (match Cc.closure_conversion_result ast with
     | Error e -> Format.eprintf "%a\n" Cc.pp_error e
     | Ok lst -> Format.printf "%a\n" pp_structure lst)
;;

let%expect_test "captured_lambda_in_nonrec_let" =
  run
    {|
  let mk_adder a =
    let add b = a + b in
    add
  ;;
  |};
  [%expect {| let mk_adder = fun a -> let add = fun a b -> (a + b) in add a;; |}]
;;

let%expect_test "top_level_nonrec_and_group" =
  run
    {|
  let f x =
    let id y = y
    and addk z = x + z in
    id 3 + addk 4
  ;;
  |};
  [%expect
    {|
    let f = fun x -> let id = fun y -> y
    and addk = fun x z -> (x + z) in (id 3 + addk x 4);; |}]
;;

let%expect_test "recursive_and_with_external_capture" =
  run
    {|
  let solve bound =
    let rec go n = if n <= bound then true else stop (n - 1)
    and stop n = if n > bound then false else go (n - 1) in
    go 20
  ;;
  |};
  [%expect
    {|
    let solve = fun bound -> let rec go = fun bound n -> if (n <= bound) then true else stop bound ((n - 1))
    and stop = fun bound n -> if (n > bound) then false else go bound ((n - 1)) in go bound 20;; |}]
;;

let%expect_test "recursive_local_function_value_capture" =
  run
    {|
  let run x =
    let rec plus y = x + y
    and call c = c + plus 7 in
    plus 1 + call 2
  ;;
  |};
  [%expect
    {|
    let run = fun x -> let rec plus = fun x y -> (x + y)
    and call = fun x c -> (c + plus x 7) in (plus x 1 + call x 2);; |}]
;;

let%expect_test "nested_levels_of_captures" =
  run
    {|
  let outer x =
    let middle y =
      let inner z =
        let deepest w = x + y + z + w in
        deepest 1
      in
      inner 2
    in
    middle 3
  ;;
  |};
  [%expect
    {| let outer = fun x -> let middle = fun x y -> let inner = fun x y z -> let deepest = fun x y z w -> (((x + y) + z) + w) in deepest x y z 1 in inner x y 2 in middle x 3;; |}]
;;

let%expect_test "if_with_lambda_in_both_branches" =
  run
    {|
  let choose flag base alt =
    if flag then (fun v -> base + v) else (fun v -> alt + v)
  |};
  [%expect
    {| let choose = fun flag base alt -> if flag then (fun base v -> (base + v)) base else (fun alt v -> (alt + v)) alt;; |}]
;;

let%expect_test "match_with_option_lambda_capture" =
  run
    {|
  let mapper x =
    match x with
    | Some y -> fun z -> y + z
    | None -> fun z -> z
  ;;
  |};
  [%expect
    {| let mapper = fun x -> match x with | Some (y) -> (fun y z -> (y + z)) y | None -> fun z -> z;; |}]
;;

let%expect_test "sequence_and_tuple_pattern_capture" =
  run
    {|
  let consume a b =
    print_int a;
    let use_pair (x, y) = a + b + x + y in
    use_pair (3, 4)
  ;;
  |};
  [%expect
    {| let consume = fun a b -> let () = print_int a in let use_pair = fun a b (x, y) -> (((a + b) + x) + y) in use_pair a b ((3, 4));; |}]
;;

let%expect_test "list_and_option_expressions" =
  run
    {|
  let build seed =
    let f x = Some (seed + x) in
    [f 1; f 2]
  ;;
  |};
  [%expect
    {| let build = fun seed -> let f = fun seed x -> Some ((seed + x)) in f seed 1::f seed 2::[];; |}]
;;

let%expect_test "type_annotation_inside_capture" =
  run
    {|
  let annotated base =
    let g x = ((base + x) : int) in
    g 5
  ;;
  |};
  [%expect
    {| let annotated = fun base -> let g = fun base x -> ((base + x) : int) in g base 5;; |}]
;;
