open EML_lib.Frontend.Parser
open EML_lib.Frontend.Ast

let parse_test input =
  match parse input with
  | Ok ast -> Printf.printf "%s\n" (show_program ast)
  | Error fail -> Printf.printf "Ошибка: %s\n" fail
;;

let%expect_test "factorial" =
  parse_test
    {| let rec fac n =
  if n <= 1
  then 1
  else let n1 = n-1 in
       let m = fac n1 in
       n*m

let main = fac 4 |};
  [%expect
    {|
      [(SValue (Rec,
          ((PatVariable "fac"),
           (ExpLambda ([(PatVariable "n")],
              (ExpBranch (
                 (ExpBinOper (LowestEqual, (ExpIdent "n"), (ExpConst (ConstInt 1))
                    )),
                 (ExpConst (ConstInt 1)),
                 (Some (ExpLet (NonRec,
                          ((PatVariable "n1"),
                           (ExpBinOper (Minus, (ExpIdent "n"),
                              (ExpConst (ConstInt 1))))),
                          [],
                          (ExpLet (NonRec,
                             ((PatVariable "m"),
                              (ExpFunction ((ExpIdent "fac"), (ExpIdent "n1")))),
                             [],
                             (ExpBinOper (Multiply, (ExpIdent "n"), (ExpIdent "m")
                                ))
                             ))
                          )))
                 ))
              ))),
          []));
        (SValue (NonRec,
           ((PatVariable "main"),
            (ExpFunction ((ExpIdent "fac"), (ExpConst (ConstInt 4))))),
           []))
        ]
|}]
;;
