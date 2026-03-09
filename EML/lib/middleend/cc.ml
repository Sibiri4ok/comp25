(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Frontend.Ast
open Runtime.Primitives
module VarSet = Set.Make (String)
module EnvMap = Map.Make (String)

let union_map_list f list =
  List.fold_left (fun acc x -> VarSet.union acc (f x)) VarSet.empty list
;;

let var_set_of_list lst = List.fold_left (fun s x -> VarSet.add x s) VarSet.empty lst

let vars_in_pattern p =
  let rec walk = function
    | PatAny -> VarSet.empty
    | PatVariable x -> VarSet.singleton x
    | PatConst _ -> VarSet.empty
    | PatTuple (p1, p2, rest) -> union_map_list walk (p1 :: p2 :: rest)
    | PatConstruct (_, None) -> VarSet.empty
    | PatConstruct (_, Some q) -> walk q
    | PatType (q, _) -> walk q
    | PatUnit | PatList _ | PatOption _ -> VarSet.empty
  in
  walk p
;;

let rec collect_free_vars = function
  | ExpIdent v -> VarSet.singleton v
  | ExpConst _ -> VarSet.empty
  | ExpLet (flag, (pat, exp), binds, body) ->
    let all_binds = (pat, exp) :: binds in
    let bound_vars = union_map_list (fun (p, _) -> vars_in_pattern p) all_binds in
    let free_vars_in_rhs =
      match flag with
      | Rec ->
        union_map_list
          (fun (_, e) -> VarSet.diff (collect_free_vars e) bound_vars)
          all_binds
      | NonRec -> union_map_list (fun (_, e) -> collect_free_vars e) all_binds
    in
    VarSet.union free_vars_in_rhs (VarSet.diff (collect_free_vars body) bound_vars)
  | ExpLambda (pat, pats, exp) ->
    let bound_vars = union_map_list vars_in_pattern (pat :: pats) in
    VarSet.diff (collect_free_vars exp) bound_vars
  | ExpApply (e1, e2) -> VarSet.union (collect_free_vars e1) (collect_free_vars e2)
  | ExpFunction ((pat, exp), cases) ->
    let one (p, e) = VarSet.diff (collect_free_vars e) (vars_in_pattern p) in
    union_map_list one ((pat, exp) :: cases)
  | ExpMatch (e, (pat, branch), cases) ->
    let one (p, e) = VarSet.diff (collect_free_vars e) (vars_in_pattern p) in
    let in_branches = union_map_list one ((pat, branch) :: cases) in
    VarSet.union (collect_free_vars e) in_branches
  | ExpBranch (cond, then_e, else_opt) ->
    union_map_list
      collect_free_vars
      (cond
       :: then_e
       ::
       (match else_opt with
        | None -> []
        | Some e -> [ e ]))
  | ExpTuple (e1, e2, rest) -> union_map_list collect_free_vars (e1 :: e2 :: rest)
  | ExpConstruct (_, None) -> VarSet.empty
  | ExpConstruct (_, Some e) -> collect_free_vars e
  | ExpTypeAnnotation (e, _) -> collect_free_vars e
  | ExpBinOper (_, e1, e2) -> VarSet.union (collect_free_vars e1) (collect_free_vars e2)
  | ExpUnarOper (_, e) -> collect_free_vars e
  | ExpList es -> union_map_list collect_free_vars es
  | ExpOption e_opt ->
    (match e_opt with
     | None -> VarSet.empty
     | Some e -> collect_free_vars e)
;;

type context =
  { globals : VarSet.t
  ; env : VarSet.t EnvMap.t
  }

let with_globals ctx g = { ctx with globals = g }
let with_env ctx e = { ctx with env = e }

type error = LambdaWithoutParameters

let pp_error ppf = function
  | LambdaWithoutParameters -> fprintf ppf "closure_conversion: lambda without parameters"
;;

type 'a t = context -> ('a, error) Result.t

let return (value : 'a) : 'a t = fun _ -> Ok value
let fail (error : error) : 'a t = fun _ -> Error error

let bind (computation : 'a t) (next : 'a -> 'b t) : 'b t =
  fun ctx ->
  match computation ctx with
  | Ok a -> next a ctx
  | Error e -> Error e
;;

let ask = fun ctx -> Ok ctx
let local f m = fun ctx -> m (f ctx)
let run ctx m = m ctx
let ( let* ) = bind

let of_result = function
  | Ok x -> return x
  | Error e -> fail e
;;

let extend_capture_env env pat captured_set =
  let rec add_captures_for_pat acc = function
    | PatAny | PatConst _ | PatConstruct (_, None) -> acc
    | PatVariable name -> EnvMap.add name captured_set acc
    | PatTuple (p1, p2, rest) ->
      let acc = add_captures_for_pat acc p1 in
      let acc = add_captures_for_pat acc p2 in
      List.fold_left add_captures_for_pat acc rest
    | PatConstruct (_, Some p) | PatType (p, _) -> add_captures_for_pat acc p
    | PatUnit | PatList _ | PatOption _ -> acc
  in
  add_captures_for_pat env pat
;;

let rec build_closure ~apply param_list body_ast captured_ids =
  let* body_ast' = convert_expr body_ast in
  let make_lam first rest = ExpLambda (first, rest, body_ast') in
  match param_list with
  | [] -> fail LambdaWithoutParameters
  | first :: rest_params ->
    if VarSet.is_empty captured_ids
    then return (make_lam first rest_params)
    else (
      let captured_list = VarSet.elements captured_ids in
      let all_params =
        List.map (fun id -> PatVariable id) captured_list @ (first :: rest_params)
      in
      let lam = make_lam (List.hd all_params) (List.tl all_params) in
      return
        (if apply
         then List.fold_left (fun t id -> ExpApply (t, ExpIdent id)) lam captured_list
         else lam))

and convert_expr = function
  | ExpIdent id ->
    let* current_ctx = ask in
    return
      (try
         let env_fvs = EnvMap.find id current_ctx.env in
         List.fold_left
           (fun t fv -> ExpApply (t, ExpIdent fv))
           (ExpIdent id)
           (VarSet.elements env_fvs)
       with
       | Not_found -> ExpIdent id)
  | ExpConst c -> return (ExpConst c)
  | ExpLet (flag, (pat, exp), more, body) ->
    let* (pat', exp'), rest_binds, body_ctx = convert_let_bindings flag (pat, exp) more in
    let* body' = local (fun _ -> body_ctx) (convert_expr body) in
    return (ExpLet (flag, (pat', exp'), rest_binds, body'))
  | ExpLambda (pat, pats, body) as lam ->
    let* current_ctx = ask in
    let param_list = pat :: pats in
    let captured = VarSet.diff (collect_free_vars lam) current_ctx.globals in
    build_closure ~apply:true param_list body captured
  | ExpApply (f, arg) ->
    let* f' = convert_expr f in
    let* arg' = convert_expr arg in
    return (ExpApply (f', arg'))
  | ExpFunction ((pat, exp), cases) ->
    let* first_exp = convert_expr exp in
    let* rest_cases =
      List.fold_right
        (fun (p, e) acc ->
           let* e' = convert_expr e in
           let* rest = acc in
           return ((p, e') :: rest))
        cases
        (return [])
    in
    return (ExpFunction ((pat, first_exp), rest_cases))
  | ExpMatch (e, (pat, branch), cases) ->
    let* scrutinee' = convert_expr e in
    let* branch' = convert_expr branch in
    let* rest_cases =
      List.fold_right
        (fun (p, e) acc ->
           let* e' = convert_expr e in
           let* rest = acc in
           return ((p, e') :: rest))
        cases
        (return [])
    in
    return (ExpMatch (scrutinee', (pat, branch'), rest_cases))
  | ExpBranch (cond, then_e, else_opt) ->
    let* cond' = convert_expr cond in
    let* then_e' = convert_expr then_e in
    let* else_e' =
      match else_opt with
      | None -> return None
      | Some e ->
        let* e' = convert_expr e in
        return (Some e')
    in
    return (ExpBranch (cond', then_e', else_e'))
  | ExpTuple (e1, e2, rest) ->
    let* e1' = convert_expr e1 in
    let* e2' = convert_expr e2 in
    let* rest' =
      List.fold_right
        (fun e acc ->
           let* e' = convert_expr e in
           let* rest_acc = acc in
           return (e' :: rest_acc))
        rest
        (return [])
    in
    return (ExpTuple (e1', e2', rest'))
  | ExpConstruct (_, None) as e -> return e
  | ExpConstruct (tag, Some e) ->
    let* e' = convert_expr e in
    return (ExpConstruct (tag, Some e'))
  | ExpTypeAnnotation (e, typ) ->
    let* e' = convert_expr e in
    return (ExpTypeAnnotation (e', typ))
  | ExpBinOper (op, e1, e2) ->
    let* e1' = convert_expr e1 in
    let* e2' = convert_expr e2 in
    return (ExpBinOper (op, e1', e2'))
  | ExpUnarOper (op, e) ->
    let* e' = convert_expr e in
    return (ExpUnarOper (op, e'))
  | ExpList es ->
    let* es' =
      List.fold_right
        (fun e acc ->
           let* e' = convert_expr e in
           let* acc' = acc in
           return (e' :: acc'))
        es
        (return [])
    in
    return (ExpList es')
  | ExpOption e_opt ->
    (match e_opt with
     | None -> return (ExpOption None)
     | Some e ->
       let* e' = convert_expr e in
       return (ExpOption (Some e')))

and convert_let_bindings rec_flag (pat, exp) rest_binds =
  let* current_ctx = ask in
  let bind_group = (pat, exp) :: rest_binds in
  let bound_ids = union_map_list (fun (p, _) -> vars_in_pattern p) bind_group in
  match rec_flag with
  | Rec ->
    let globals' = VarSet.union current_ctx.globals bound_ids in
    let group_captured =
      union_map_list (fun (_, e) -> VarSet.diff (collect_free_vars e) globals') bind_group
    in
    let env' =
      List.fold_left
        (fun acc (p, _) -> extend_capture_env acc p group_captured)
        current_ctx.env
        bind_group
    in
    let rec_group_ctx = with_env (with_globals current_ctx globals') env' in
    let rec loop acc = function
      | [] -> return (List.rev acc)
      | (p, e) :: rest ->
        let fvs = VarSet.diff group_captured (vars_in_pattern p) in
        let res =
          match e with
          | ExpLambda (lam_pat, lam_pats, body) ->
            run rec_group_ctx (build_closure ~apply:false (lam_pat :: lam_pats) body fvs)
          | _ -> run rec_group_ctx (convert_expr e)
        in
        let* e' = of_result res in
        loop ((p, e') :: acc) rest
    in
    let* transformed_binds = loop [] bind_group in
    return (List.hd transformed_binds, List.tl transformed_binds, rec_group_ctx)
  | NonRec ->
    let rec loop env_acc rev_binds = function
      | [] ->
        let transformed_binds = List.rev rev_binds in
        return
          ( List.hd transformed_binds
          , List.tl transformed_binds
          , with_env current_ctx env_acc )
      | (p, e) :: rest ->
        let captured = VarSet.diff (collect_free_vars e) current_ctx.globals in
        let ctx_with_env = { current_ctx with env = env_acc } in
        let res =
          match e with
          | ExpLambda (lam_pat, lam_pats, body) ->
            run
              ctx_with_env
              (build_closure ~apply:false (lam_pat :: lam_pats) body captured)
          | _ -> run ctx_with_env (convert_expr e)
        in
        let* e' = of_result res in
        let env_next =
          match e with
          | ExpLambda _ -> extend_capture_env env_acc p captured
          | _ -> env_acc
        in
        loop env_next ((p, e') :: rev_binds) rest
    in
    loop current_ctx.env [] bind_group
;;

let convert_item = function
  | SEval expr ->
    let* e' = convert_expr expr in
    let* current_ctx = ask in
    return (current_ctx.globals, SEval e')
  | SValue (rec_flag, (pat, expr), and_binds) ->
    let* (pat', expr'), rest_binds, after_binds_ctx =
      convert_let_bindings rec_flag (pat, expr) and_binds
    in
    let bound_ids =
      union_map_list (fun (p, _) -> vars_in_pattern p) ((pat, expr) :: and_binds)
    in
    return
      ( VarSet.union after_binds_ctx.globals bound_ids
      , SValue (rec_flag, (pat', expr'), rest_binds) )
;;

let builtin_globals =
  var_set_of_list (List.map (fun f -> f.name) predefined_runtime_funcs)
;;

let initial_context = { globals = builtin_globals; env = EnvMap.empty }

let closure_conversion_result (program : Frontend.Ast.program)
  : (structure list, error) Result.t
  =
  let rec convert_items rev_acc item_ctx = function
    | [] -> Ok (List.rev rev_acc)
    | item :: tail ->
      (match run item_ctx (convert_item item) with
       | Ok (globals', item') ->
         convert_items (item' :: rev_acc) { globals = globals'; env = EnvMap.empty } tail
       | Error e -> Error e)
  in
  convert_items [] initial_context program
;;
