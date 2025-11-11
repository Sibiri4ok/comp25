(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib.Frontend.Parser
open EML_lib.Middleend.Anf

let parse_and_anf input =
  match parse input with
  | Ok ast ->
    (match anf_program ast with
     | Ok anf_ast -> Printf.printf "%s\n" (show_aprogram anf_ast)
     | Error e -> Printf.printf "ANF error: %s\n" e)
  | Error e -> Printf.printf "Parsing error: %s\n" e
;;

let%expect_test "simple_arithmetic" =
  parse_and_anf "let rec fac n = if n<=1 then 1 else n * fac (n-1)";
  [%expect
    {|
    [AEval
      (ALet (NonRec, "anf_t1",
         CBinop (Multiply, ImmConst (ConstInt 2), ImmConst (ConstInt 3)),
         ALet (NonRec, "anf_t2",
            CBinop (Plus, ImmConst (ConstInt 1), ImmVar "anf_t1"),
            ACExpr (CImm (ImmVar "anf_t2")))))]|}]
;;
