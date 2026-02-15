(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib.Frontend.Parser
open EML_lib.Middleend.Anf
open EML_lib.Middleend.Anf_pp

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
     (AnfExpr
        (ComplexLambda ([(PatVariable "n")],
           (AnfLet (NonRec, "anf_t0",
              (ComplexBinOper (LowestEqual, (ImmediateVar "n"),
                 (ImmediateConst (ConstInt 1)))),
              (AnfExpr
                 (ComplexBranch ((ImmediateVar "anf_t0"),
                    (AnfExpr (ComplexImmediate (ImmediateConst (ConstInt 1)))),
                    (AnfLet (NonRec, "anf_t1",
                       (ComplexBinOper (Minus, (ImmediateVar "n"),
                          (ImmediateConst (ConstInt 1)))),
                       (AnfLet (NonRec, "anf_t2",
                          (ComplexApp ((ImmediateVar "fac"),
                             (ImmediateVar "anf_t1"), [])),
                          (AnfExpr
                             (ComplexBinOper (Multiply, (ImmediateVar "n"),
                                (ImmediateVar "anf_t2"))))
                          ))
                       ))
                    )))
              ))
           )))),
    []))
  ]|}]
;;

let%expect_test "003occurs.ml" =
  parse_and_anf "let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f))";
  [%expect
    {|
[(AnfValue (NonRec,
    ("fix",
     (AnfExpr
        (ComplexLambda ([(PatVariable "f")],
           (AnfExpr
              (ComplexLambda ([(PatVariable "x")],
                 (AnfExpr
                    (ComplexLambda ([(PatVariable "f")],
                       (AnfLet (NonRec, "anf_t1",
                          (ComplexApp ((ImmediateVar "x"),
                             (ImmediateVar "x"), [(ImmediateVar "f")])),
                          (AnfLet (NonRec, "anf_t3",
                             (ComplexApp ((ImmediateVar "f"),
                                (ImmediateVar "anf_t1"), [])),
                             (AnfExpr
                                (ComplexLambda ([(PatVariable "x")],
                                   (AnfExpr
                                      (ComplexLambda ([(PatVariable "f")],
                                         (AnfLet (NonRec, "anf_t5",
                                            (ComplexApp ((ImmediateVar "x"),
                                               (ImmediateVar "x"),
                                               [(ImmediateVar "f")])),
                                            (AnfLet (NonRec, "anf_t7",
                                               (ComplexApp (
                                                  (ImmediateVar "f"),
                                                  (ImmediateVar "anf_t5"),
                                                  [])),
                                               (AnfExpr
                                                  (ComplexApp (
                                                     (ImmediateVar "anf_t3"),
                                                     (ImmediateVar "anf_t7"),
                                                     [])))
                                               ))
                                            ))
                                         )))
                                   )))
                             ))
                          ))
                       )))
                 )))
           )))),
    []))
  ]|}]
;;

let%expect_test "004let_poly.ml" =
  parse_and_anf "let temp =\n  (fun f -> (f 1, f true)) (fun x -> x)";
  [%expect
    {|
[(AnfValue (NonRec,
    ("temp",
     (AnfExpr
        (ComplexLambda ([(PatVariable "f")],
           (AnfLet (NonRec, "anf_t0",
              (ComplexApp ((ImmediateVar "f"), (ImmediateConst (ConstInt 1)),
                 [])),
              (AnfLet (NonRec, "anf_t1",
                 (ComplexApp ((ImmediateVar "f"),
                    (ImmediateConst (ConstBool true)), [])),
                 (AnfLet (NonRec, "anf_t3",
                    (ComplexTuple ((ImmediateVar "anf_t0"),
                       (ImmediateVar "anf_t1"), [])),
                    (AnfExpr
                       (ComplexLambda ([(PatVariable "x")],
                          (AnfLet (NonRec, "anf_t4",
                             (ComplexImmediate (ImmediateVar "x")),
                             (AnfExpr
                                (ComplexApp ((ImmediateVar "anf_t3"),
                                   (ImmediateVar "anf_t4"), [])))
                             ))
                          )))
                    ))
                 ))
              ))
           )))),
    []))
  ]|}]
;;

let%expect_test "002if.ml" =
  parse_and_anf "let main = if true then 1 else false";
  [%expect
    {|
  [(AnfValue (NonRec,
      ("main",
       (AnfExpr
          (ComplexBranch ((ImmediateConst (ConstBool true)),
             (AnfExpr (ComplexImmediate (ImmediateConst (ConstInt 1)))),
             (AnfExpr (ComplexImmediate (ImmediateConst (ConstBool false)))))))),
      []))
    ]|}]
;;

let%expect_test "pretty_printer_test1" =
  parse_and_anf_pp
    "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)\n  let main = fac 4";
  [%expect
    {|
      let rec fac = fun n -> let anf_t0 = (n <= 1) in
      if anf_t0 then 1 else let anf_t1 = (n - 1) in let anf_t2 = fac anf_t1 in
      (n * anf_t2)

      let main = fac 4 |}]
;;

let%expect_test "pretty_printer_test2" =
  parse_and_anf_pp
    "let rec fibo = fun n -> if n < 1 then 1 else fibo (n-1) + fibo (n-2)\n\
    \  let main = fibo 10";
  [%expect
    {|
      let rec fibo = fun n -> let anf_t0 = (n < 1) in
      if anf_t0 then 1 else let anf_t1 = (n - 1) in let anf_t2 = fibo anf_t1 in
      let anf_t3 = (n - 2) in let anf_t4 = fibo anf_t3 in
      (anf_t2 + anf_t4)

      let main = fibo 10|}]
;;
