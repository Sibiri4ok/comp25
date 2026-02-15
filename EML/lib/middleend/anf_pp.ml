(* Pretty-printer for ANF expressions *)
open Stdlib.Format
open Frontend
open Ast
open Anf

let pp_ty = Frontend.Ast.pp_ty

let rec pp_immediate fmt = function
  | ImmediateConst c ->
    (match c with
     | ConstInt n -> fprintf fmt "%d" n
     | ConstBool b -> fprintf fmt "%b" b
     | ConstString s -> fprintf fmt "\"%s\"" s
     | ConstChar ch -> fprintf fmt "'%s'" (Char.escaped ch))
  | ImmediateVar x -> fprintf fmt "%s" x

and pp_complex_expr fmt = function
  | ComplexImmediate imm -> pp_immediate fmt imm
  | ComplexUnit -> fprintf fmt "()"
  | ComplexField (imm, i) -> fprintf fmt "%a.%d" pp_immediate imm i
  | ComplexBinOper (op, e1, e2) ->
    let op_str =
      match op with
      | Plus -> "+"
      | Minus -> "-"
      | Multiply -> "*"
      | Division -> "/"
      | And -> "&&"
      | Or -> "||"
      | GretestEqual -> ">="
      | LowestEqual -> "<="
      | GreaterThan -> ">"
      | LowerThan -> "<"
      | Equal -> "="
      | NotEqual -> "<>"
    in
    fprintf fmt "(%a %s %a)" pp_immediate e1 op_str pp_immediate e2
  | ComplexUnarOper (op, e) ->
    let op_str =
      match op with
      | Negative -> "-"
      | Not -> "not"
    in
    fprintf fmt "(%s %a)" op_str pp_immediate e
  | ComplexTuple (e1, e2, rest) ->
    let all_exprs = e1 :: e2 :: rest in
    fprintf
      fmt
      "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_immediate)
      all_exprs
  | ComplexList exprs ->
    fprintf
      fmt
      "[%a]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_immediate)
      exprs
  | ComplexOption None -> fprintf fmt "None"
  | ComplexOption (Some e) -> fprintf fmt "Some %a" pp_immediate e
  | ComplexApp (f, arg, args) ->
    let all_args = arg :: args in
    fprintf
      fmt
      "%a %a"
      pp_immediate
      f
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_immediate)
      all_args
  | ComplexLambda (patterns, body) ->
    let pp_pattern fmt pat =
      match pat with
      | PatVariable x -> fprintf fmt "%s" x
      | PatConst c ->
        (match c with
         | ConstInt n -> fprintf fmt "%d" n
         | ConstBool b -> fprintf fmt "%b" b
         | ConstString s -> fprintf fmt "\"%s\"" s
         | ConstChar ch -> fprintf fmt "'%s'" (Char.escaped ch))
      | PatTuple (p1, p2, rest) ->
        let all_pats = p1 :: p2 :: rest in
        fprintf
          fmt
          "(%a)"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_pattern)
          all_pats
      | PatAny -> fprintf fmt "_"
      | PatType (p, t) -> fprintf fmt "%a : %a" pp_pattern p pp_ty t
      | PatUnit -> fprintf fmt "()"
      | PatList pats ->
        fprintf
          fmt
          "[%a]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_pattern)
          pats
      | PatOption None -> fprintf fmt "None"
      | PatOption (Some p) -> fprintf fmt "Some %a" pp_pattern p
      | PatConstruct (name, opt) ->
        (match opt with
         | None -> fprintf fmt "%s" name
         | Some p -> fprintf fmt "%s %a" name pp_pattern p)
    in
    fprintf
      fmt
      "fun %a -> %a"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_pattern)
      patterns
      pp_anf_expr
      body
  | ComplexBranch (cond, then_expr, else_expr) ->
    fprintf
      fmt
      "if %a then %a else %a"
      pp_immediate
      cond
      pp_anf_expr
      then_expr
      pp_anf_expr
      else_expr

and pp_anf_expr fmt = function
  | AnfLet (rf, name, v, body) ->
    let rec_flag =
      match rf with
      | Rec -> "rec "
      | NonRec -> ""
    in
    fprintf fmt "let %s%s = %a in@ %a" rec_flag name pp_complex_expr v pp_anf_expr body
  | AnfExpr e -> pp_complex_expr fmt e

and pp_anf_bind fmt (name, expr) = fprintf fmt "%s = %a" name pp_anf_expr expr

and pp_anf_structure fmt = function
  | AnfEval expr -> fprintf fmt "%a" pp_anf_expr expr
  | AnfValue (rf, bind, binds) ->
    let rec_flag =
      match rf with
      | Rec -> "rec "
      | NonRec -> ""
    in
    let all_binds = bind :: binds in
    fprintf
      fmt
      "let %s%a"
      rec_flag
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ and ") pp_anf_bind)
      all_binds

and pp_anf_program fmt program =
  fprintf
    fmt
    "%a"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n\n") pp_anf_structure)
    program
;;

let anf_to_string anf_program = Stdlib.Format.asprintf "%a" pp_anf_program anf_program
