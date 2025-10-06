(** Copyright 2024-2025, Danil Usoltsev *)

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
       Base.Map.iteri filtered_env ~f:(fun ~key ~data:(S (_, ty)) ->
         Format.printf "val %s: %a\n" key pp_ty ty)
     | Error e -> Format.printf "Infer error. %a\n" pp_error e)
  | Error e -> Format.printf "Parsing error. %s\n" e
;;

(* let pretty_printer_parse_and_infer_simple s =
  match parse s with
  | Ok parsed ->
    (match parsed with
     | [ SEval expr ] ->
       (match infer_simple_expression expr with
        | Ok ty -> Format.printf "%a\n" pp_ty ty
        | Error e -> Format.printf "Infer error. %a\n" pp_error e)
     | _ ->
       Format.printf
         "Expected a single expression, but got a program with multiple structures.\n")
  | Error e -> Format.printf "Parsing error. %s\n" e
;; *)

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
