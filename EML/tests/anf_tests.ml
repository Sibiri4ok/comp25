(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Emlib.Frontend.Parser
open Emlib.Middleend
open Anf
open Anf_pp
open Anf_tree

let parse_and_anf input =
  match parse input with
  | Ok ast ->
    (match anf_program ast with
     | Ok anf_ast -> Printf.printf "%s\n" (show_anf_program anf_ast)
     | Error e -> Printf.printf "ANF error: %s\n" e)
  | Error e -> Printf.printf "Parsing error: %s\n" e
;;

let parse_and_anf_pp input =
  match parse input with
  | Ok ast ->
    (match anf_program ast with
     | Ok anf_ast -> Printf.printf "%s\n" (anf_to_string anf_ast)
     | Error e -> Printf.printf "ANF error: %s\n" e)
  | Error e -> Printf.printf "Parsing error: %s\n" e
;;

let%expect_test "001.ml" =
  parse_and_anf "let recfac n = if n<=1 then 1 else n * fac (n-1)";
  [%expect
    {|
[(AnfValue (NonRec,
    ("recfac",
     (AnfLet (NonRec, "anf_t5",
        (ComplexLambda ([(PatVariable "n")],
           (AnfLet (NonRec, "anf_t0",
              (ComplexBinOper (LowestEqual, (ImmediateVar "n"),
                 (ImmediateConst (ConstInt 1)))),
              (AnfLet (NonRec, "anf_t1",
                 (ComplexBranch ((ImmediateVar "anf_t0"),
                    (AnfExpr (ComplexImmediate (ImmediateConst (ConstInt 1)))),
                    (AnfLet (NonRec, "anf_t2",
                       (ComplexBinOper (Minus, (ImmediateVar "n"),
                          (ImmediateConst (ConstInt 1)))),
                       (AnfLet (NonRec, "anf_t3",
                          (ComplexApp ((ImmediateVar "fac"),
                             (ImmediateVar "anf_t2"), [])),
                          (AnfLet (NonRec, "anf_t4",
                             (ComplexBinOper (Multiply, (ImmediateVar "n"),
                                (ImmediateVar "anf_t3"))),
                             (AnfExpr
                                (ComplexImmediate (ImmediateVar "anf_t4")))
                             ))
                          ))
                       ))
                    )),
                 (AnfExpr (ComplexImmediate (ImmediateVar "anf_t1")))))
              ))
           )),
        (AnfExpr (ComplexImmediate (ImmediateVar "anf_t5")))))),
    []))
  ]|}]
;;

let%expect_test "003occurs.ml" =
  parse_and_anf "let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f))";
  [%expect
    {|
[(AnfValue (NonRec,
    ("fix",
     (AnfLet (NonRec, "anf_t11",
        (ComplexLambda ([(PatVariable "f")],
           (AnfLet (NonRec, "anf_t4",
              (ComplexLambda ([(PatVariable "x")],
                 (AnfLet (NonRec, "anf_t2",
                    (ComplexLambda ([(PatVariable "f")],
                       (AnfLet (NonRec, "anf_t0",
                          (ComplexApp ((ImmediateVar "x"),
                             (ImmediateVar "x"), [])),
                          (AnfLet (NonRec, "anf_t1",
                             (ComplexApp ((ImmediateVar "anf_t0"),
                                (ImmediateVar "f"), [])),
                             (AnfExpr
                                (ComplexImmediate (ImmediateVar "anf_t1")))
                             ))
                          ))
                       )),
                    (AnfLet (NonRec, "anf_t3",
                       (ComplexApp ((ImmediateVar "f"),
                          (ImmediateVar "anf_t2"), [])),
                       (AnfExpr (ComplexImmediate (ImmediateVar "anf_t3")))))
                    ))
                 )),
              (AnfLet (NonRec, "anf_t9",
                 (ComplexLambda ([(PatVariable "x")],
                    (AnfLet (NonRec, "anf_t7",
                       (ComplexLambda ([(PatVariable "f")],
                          (AnfLet (NonRec, "anf_t5",
                             (ComplexApp ((ImmediateVar "x"),
                                (ImmediateVar "x"), [])),
                             (AnfLet (NonRec, "anf_t6",
                                (ComplexApp ((ImmediateVar "anf_t5"),
                                   (ImmediateVar "f"), [])),
                                (AnfExpr
                                   (ComplexImmediate (ImmediateVar "anf_t6")))
                                ))
                             ))
                          )),
                       (AnfLet (NonRec, "anf_t8",
                          (ComplexApp ((ImmediateVar "f"),
                             (ImmediateVar "anf_t7"), [])),
                          (AnfExpr (ComplexImmediate (ImmediateVar "anf_t8")))
                          ))
                       ))
                    )),
                 (AnfLet (NonRec, "anf_t10",
                    (ComplexApp ((ImmediateVar "anf_t4"),
                       (ImmediateVar "anf_t9"), [])),
                    (AnfExpr (ComplexImmediate (ImmediateVar "anf_t10")))))
                 ))
              ))
           )),
        (AnfExpr (ComplexImmediate (ImmediateVar "anf_t11")))))),
    []))
  ]|}]
;;

let%expect_test "004let_poly.ml" =
  parse_and_anf "let temp =\n  (fun f -> (f 1, f true)) (fun x -> x)";
  [%expect
    {|
[(AnfValue (NonRec,
    ("temp",
     (AnfLet (NonRec, "anf_t3",
        (ComplexLambda ([(PatVariable "f")],
           (AnfLet (NonRec, "anf_t0",
              (ComplexApp ((ImmediateVar "f"), (ImmediateConst (ConstInt 1)),
                 [])),
              (AnfLet (NonRec, "anf_t1",
                 (ComplexApp ((ImmediateVar "f"),
                    (ImmediateConst (ConstBool true)), [])),
                 (AnfLet (NonRec, "anf_t2",
                    (ComplexTuple ((ImmediateVar "anf_t0"),
                       (ImmediateVar "anf_t1"), [])),
                    (AnfExpr (ComplexImmediate (ImmediateVar "anf_t2")))))
                 ))
              ))
           )),
        (AnfLet (NonRec, "anf_t4",
           (ComplexLambda ([(PatVariable "x")],
              (AnfExpr (ComplexImmediate (ImmediateVar "x"))))),
           (AnfLet (NonRec, "anf_t5",
              (ComplexApp ((ImmediateVar "anf_t3"), (ImmediateVar "anf_t4"),
                 [])),
              (AnfExpr (ComplexImmediate (ImmediateVar "anf_t5")))))
           ))
        ))),
    []))
  ]|}]
;;

let%expect_test "002if.ml" =
  parse_and_anf "let main = if true then 1 else false";
  [%expect
    {|
  [(AnfValue (NonRec,
      ("main",
       (AnfLet (NonRec, "anf_t0",
          (ComplexBranch ((ImmediateConst (ConstBool true)),
             (AnfExpr (ComplexImmediate (ImmediateConst (ConstInt 1)))),
             (AnfExpr (ComplexImmediate (ImmediateConst (ConstBool false)))))),
          (AnfExpr (ComplexImmediate (ImmediateVar "anf_t0")))))),
      []))
    ]|}]
;;

let%expect_test "pretty_printer_test1" =
  parse_and_anf_pp
    "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)\n  let main = fac 4";
  [%expect
    {|
      let rec fac = let anf_t6 = fun n -> let anf_t1 = (n <= 1) in
      let anf_t2 = if anf_t1 then 1 else let anf_t3 = (n - 1) in
      let anf_t4 = fac anf_t3 in let anf_t5 = (n * anf_t4) in anf_t5 in anf_t2 in
      anf_t6

      let main = let anf_t0 = fac 4 in
      anf_t0 |}]
;;

let%expect_test "pretty_printer_test2" =
  parse_and_anf_pp
    "let rec fibo = fun n -> if n < 1 then 1 else fibo (n-1) + fibo (n-2)\n\
    \  let main = fibo 10";
  [%expect
    {|
      let rec fibo = let anf_t8 = fun n -> let anf_t1 = (n < 1) in
      let anf_t2 = if anf_t1 then 1 else let anf_t3 = (n - 1) in
      let anf_t4 = fibo anf_t3 in let anf_t5 = (n - 2) in
      let anf_t6 = fibo anf_t5 in let anf_t7 = (anf_t4 + anf_t6) in anf_t7 in
      anf_t2 in anf_t8

      let main = let anf_t0 = fibo 10 in 
      anf_t0|}]
;;
