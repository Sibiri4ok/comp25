(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib.Frontend.Inferencer
open EML_lib.Frontend.Ast
open EML_lib.Frontend.Parser

let pretty_printer_parse_and_infer s =
  match parse s with
  | Ok parsed ->
    (match run_infer parsed with
     | Ok env ->
       let filtered_env =
         Base.Map.filter_keys env ~f:(fun key ->
           not (List.mem key [ "print_int"; "print_endline"; "print_bool" ]))
       in
       Base.Map.iteri filtered_env ~f:(fun ~key ~data:(Scheme.Scheme (_, ty)) ->
         Format.printf "val %s: %a\n" key pp_ty ty)
     | Error e -> Format.printf "Infer error. %a\n" pp_error e)
  | Error e -> Format.printf "Parsing error. %s\n" e
;;


let%expect_test "test_factorial" =
  pretty_printer_parse_and_infer
    {| let rec fac n =
  if n <= 1
  then 1
  else let n1 = n-1 in
       let m = fac n1 in
       n*m

let main = fac 4 |};
  [%expect
    {|
    val fac: int -> int
    val main: int|}]
;;

let%expect_test "test_primitives_and_data" =
  pretty_printer_parse_and_infer
    {| let a = 1 + 2
let b = true && false
let c = if true then 1 else 2
let d = (1, true, 'a')
let e = [1; 2; 3]
let f = Some 1
let g = None
|};
  [%expect
    {|
    val a: int
    val b: bool
    val c: int
    val d: (int * bool * char)
    val e: int list
    val f: int option
    val g: t1 option|}]
;;

let%expect_test "test_match_and_recursion" =
  pretty_printer_parse_and_infer
    {| let rec len xs =
  match xs with
  | [] -> 0
  | _::tl -> 1 + len tl

let main = len [1;2;3] |};
  [%expect
    {|
    val len: int list -> int
    val main: int|}]
;;

let%expect_test "test_mutual_recursion" =
  pretty_printer_parse_and_infer
    {| let rec even n =
  if n = 0 then true else odd (n - 1)
and odd n =
  if n = 0 then false else even (n - 1)

let main = even 4 |};
  [%expect
    {|
    val even: int -> bool
    val main: bool
    val odd: int -> bool|}]
;;

let%expect_test "test_annotations" =
  pretty_printer_parse_and_infer
    {| let id = ((fun x -> x) : int -> int)
let main = id 10 |};
  [%expect
    {|
    val id: int -> int
    val main: int|}]
;;

let%expect_test "test_unbound_variable_error" =
  pretty_printer_parse_and_infer {| let main = x |};
  [%expect {|Infer error. Unbound variable 'x'.|}]
;;

let%expect_test "test_rec_rhs_error" =
  pretty_printer_parse_and_infer {| let rec x = 1 |};
  [%expect
    {|Infer error. Right-hand side error: Right-hand side of let rec must be a lambda expression.|}]
;;

let%expect_test "test_list_constructors_and_match" =
  pretty_printer_parse_and_infer
    {| let rec head_or_zero xs =
  match xs with
  | [] -> 0
  | h::tl -> h

let x = 1 :: []
let main = head_or_zero x |};
  [%expect
    {|
    val head_or_zero: int list -> int
    val main: int
    val x: int list|}]
;;

let%expect_test "test_pattern_option_and_list" =
  pretty_printer_parse_and_infer
    {| let f = function
  | Some (h::tl) -> h
|};
  [%expect {|val f: t2 list option -> t2|}]
;;

let%expect_test "test_annotation_mismatch_error" =
  pretty_printer_parse_and_infer {| let x = (1 : bool) |};
  [%expect {|Infer error. Failed to unify types: int and bool.|}]
;;

let%expect_test "test_unexpected_function_error_branch" =
  pretty_printer_parse_and_infer {| let x = not 1 |};
  [%expect {|Infer error. Failed to unify types: int and bool.|}]
;;

