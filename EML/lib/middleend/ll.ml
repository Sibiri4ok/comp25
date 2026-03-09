(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast
module StringSet = Set.Make (String)
module Map = Map.Make (String)

module type NAMING = sig
  type t

  val fresh : t -> string * t
  val init : t
end

module Default_naming : NAMING = struct
  type t = int

  let init = 0

  let fresh n =
    let s = "lifted_" ^ Int.to_string n in
    s, n + 1
  ;;
end

type lift_result =
  { structures : structure list
  ; expr : expr
  }

let names_in_pattern p =
  let rec collect = function
    | PatAny -> []
    | PatVariable s -> [ s ]
    | PatConst _ -> []
    | PatConstruct (_, None) -> []
    | PatConstruct (_, Some q) -> collect q
    | PatType (q, _) -> collect q
    | PatTuple (p1, p2, rest) -> List.concat_map collect (p1 :: p2 :: rest)
    | PatUnit -> []
    | PatList ps -> List.concat_map collect ps
    | PatOption p_opt ->
      (match p_opt with
       | None -> []
       | Some x -> collect x)
  in
  collect p
;;

let rename_pattern env p =
  let rec subst = function
    | PatVariable s ->
      let s' =
        try Map.find s env with
        | Not_found -> s
      in
      PatVariable s'
    | PatConstruct (id, p_opt) -> PatConstruct (id, Option.map subst p_opt)
    | PatType (p, t) -> PatType (subst p, t)
    | PatList ps -> PatList (List.map subst ps)
    | PatOption p_opt -> PatOption (Option.map subst p_opt)
    | other -> other
  in
  subst p
;;

let unique_names_in_bind_group binds =
  let add_if_new (rev_list, set) id =
    if StringSet.mem id set then rev_list, set else id :: rev_list, StringSet.add id set
  in
  let rev_list, _seen =
    List.fold_left
      (fun (rev_list, set) (p, _) ->
         List.fold_left add_if_new (rev_list, set) (names_in_pattern p))
      ([], StringSet.empty)
      binds
  in
  List.rev rev_list
;;

type error =
  | RecLetEmptyBinding
  | SValueEmptyBinding

let pp_error ppf = function
  | RecLetEmptyBinding ->
    Format.fprintf ppf "lambda_lifting: Rec let must have at least one binding"
  | SValueEmptyBinding ->
    Format.fprintf ppf "lambda_lifting: SValue must have at least one binding"
;;

module Make (N : NAMING) = struct
  type 'a t = N.t -> ('a * N.t, error) Result.t

  let return (x : 'a) : 'a t = fun st -> Ok (x, st)
  let fail (e : error) : _ t = fun _ -> Error e

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun st ->
    match m st with
    | Ok (x, st') -> f x st'
    | Error e -> Error e
  ;;

  let ( let* ) = bind

  let take_names k : string list t =
    fun st ->
    let rec loop acc st' i =
      if i <= 0
      then Ok (List.rev acc, st')
      else (
        let name, st'' = N.fresh st' in
        loop (name :: acc) st'' (i - 1))
    in
    loop [] st k
  ;;

  let map2 (m1 : 'a t) (m2 : 'b t) (f : 'a -> 'b -> 'c) : 'c t =
    fun st ->
    match m1 st with
    | Error e -> Error e
    | Ok (x1, st1) ->
      (match m2 st1 with
       | Error e -> Error e
       | Ok (x2, st2) -> Ok (f x1 x2, st2))
  ;;

  let pair (m1 : lift_result t) (m2 : lift_result t) (f : expr -> expr -> expr)
    : lift_result t
    =
    map2 m1 m2 (fun r1 r2 ->
      { structures = r1.structures @ r2.structures; expr = f r1.expr r2.expr })
  ;;

  let triple
        (m1 : lift_result t)
        (m2 : lift_result t)
        (m3 : lift_result t)
        (f : expr -> expr -> expr -> expr)
    : lift_result t
    =
    map2
      m1
      (map2 m2 m3 (fun r2 r3 -> r2, r3))
      (fun r1 (r2, r3) ->
         { structures = r1.structures @ r2.structures @ r3.structures
         ; expr = f r1.expr r2.expr r3.expr
         })
  ;;

  let list (exprs : expr list) (m : expr -> lift_result t)
    : (structure list * expr list) t
    =
    fun st ->
    let rec loop rev_structs rev_exprs st' = function
      | [] -> Ok ((List.concat (List.rev rev_structs), List.rev rev_exprs), st')
      | e :: rest ->
        (match m e st' with
         | Error e_err -> Error e_err
         | Ok (r, st'') ->
           loop (r.structures :: rev_structs) (r.expr :: rev_exprs) st'' rest)
    in
    loop [] [] st exprs
  ;;

  type context =
    { renames : string Map.t
    ; at_toplevel : bool
    }

  let initial_renames = Map.empty

  let without_bindings renames names =
    List.fold_left (fun m k -> Map.remove k m) renames names
  ;;

  let inner (ctx : context) = { ctx with at_toplevel = false }

  let fold_binds (ctx : context) binds (f : context -> pattern -> expr -> lift_result t)
    : (structure list * (pattern * expr) list) t
    =
    List.fold_left
      (fun acc (p, e) ->
         let* rev_structures, rev_binds = acc in
         let* res = f ctx p e in
         return (res.structures :: rev_structures, (p, res.expr) :: rev_binds))
      (return ([], []))
      binds
    |> fun m ->
    let* rev_structures, rev_binds = m in
    return (List.concat (List.rev rev_structures), List.rev rev_binds)
  ;;

  let rec lift_expr (ctx : context) (e : expr) : lift_result t =
    match e with
    | ExpIdent name ->
      let name' =
        try Map.find name ctx.renames with
        | Not_found -> name
      in
      return { structures = []; expr = ExpIdent name' }
    | (ExpConst _ | ExpConstruct (_, None)) as e -> return { structures = []; expr = e }
    | ExpLet (NonRec, (pat, exp), more, body) ->
      let* res_rhs = lift_expr (inner ctx) exp in
      let* extra_structures, rest_binds = lift_binds (inner ctx) more in
      let all_defs =
        names_in_pattern pat @ List.concat_map (fun (p, _) -> names_in_pattern p) more
      in
      let body_ctx =
        { (inner ctx) with renames = without_bindings ctx.renames all_defs }
      in
      let* res_body = lift_expr body_ctx body in
      return
        { structures = res_rhs.structures @ extra_structures @ res_body.structures
        ; expr = ExpLet (NonRec, (pat, res_rhs.expr), rest_binds, res_body.expr)
        }
    | ExpLet (Rec, (pat, exp), more, body) ->
      let rec_binds = (pat, exp) :: more in
      let unique_ids = unique_names_in_bind_group rec_binds in
      let* names = take_names (List.length unique_ids) in
      let rec_ctx =
        { (inner ctx) with
          renames =
            List.fold_left
              (fun env (id, name) -> Map.add id name env)
              ctx.renames
              (List.combine unique_ids names)
        }
      in
      let* inner_structures, lifted_binds =
        List.fold_left
          (fun acc (p, e) ->
             let* structures_acc, binds_acc = acc in
             let* res = lift_expr rec_ctx e in
             return
               ( structures_acc @ res.structures
               , binds_acc @ [ rename_pattern rec_ctx.renames p, res.expr ] ))
          (return ([], []))
          rec_binds
      in
      let* res_body = lift_expr rec_ctx body in
      let* first_bind, rest_binds =
        match lifted_binds with
        | first :: rest -> return (first, rest)
        | [] -> fail RecLetEmptyBinding
      in
      return
        { res_body with
          structures =
            inner_structures
            @ [ SValue (Rec, first_bind, rest_binds) ]
            @ res_body.structures
        }
    | ExpLambda (pat, pats, body) when ctx.at_toplevel ->
      let* res = lift_expr (inner ctx) body in
      return { res with expr = ExpLambda (pat, pats, res.expr) }
    | ExpLambda (pat, pats, body) ->
      let* names = take_names 1 in
      let name = List.hd names in
      let args = pat :: pats in
      let bound = List.concat_map names_in_pattern args in
      let* res =
        lift_expr { (inner ctx) with renames = without_bindings ctx.renames bound } body
      in
      let value_def =
        SValue (NonRec, (PatVariable name, ExpLambda (pat, pats, res.expr)), [])
      in
      return { structures = res.structures @ [ value_def ]; expr = ExpIdent name }
    | ExpApply (e1, e2) ->
      pair
        (lift_expr (inner ctx) e1)
        (lift_expr (inner ctx) e2)
        (fun e1' e2' -> ExpApply (e1', e2'))
    | ExpFunction ((pat, exp), cases) when ctx.at_toplevel ->
      let ctx_rhs =
        { (inner ctx) with renames = without_bindings ctx.renames (names_in_pattern pat) }
      in
      let* res_rhs = lift_expr ctx_rhs exp in
      let* case_structures, lifted_cases =
        lift_binds_with_pattern_scope (inner ctx) cases
      in
      return
        { structures = res_rhs.structures @ case_structures
        ; expr = ExpFunction ((pat, res_rhs.expr), lifted_cases)
        }
    | ExpFunction ((pat1, exp1), cases) ->
      let* names = take_names 1 in
      let name = List.hd names in
      let ctx_body =
        { ctx with renames = without_bindings ctx.renames (names_in_pattern pat1) }
      in
      let* res_body = lift_expr ctx_body exp1 in
      let* case_structures, lifted_cases = lift_binds_with_pattern_scope ctx cases in
      let value_def =
        SValue
          ( NonRec
          , (PatVariable name, ExpFunction ((pat1, res_body.expr), lifted_cases))
          , [] )
      in
      return
        { structures = res_body.structures @ case_structures @ [ value_def ]
        ; expr = ExpIdent name
        }
    | ExpMatch (e, (pat, branch), cases) ->
      let* res_scrut = lift_expr (inner ctx) e in
      let ctx_branch =
        { (inner ctx) with renames = without_bindings ctx.renames (names_in_pattern pat) }
      in
      let* res_branch = lift_expr ctx_branch branch in
      let* case_structures, lifted_cases = lift_binds_with_pattern_scope ctx cases in
      return
        { structures = res_scrut.structures @ res_branch.structures @ case_structures
        ; expr = ExpMatch (res_scrut.expr, (pat, res_branch.expr), lifted_cases)
        }
    | ExpBranch (e1, e2, e3_opt) ->
      (match e3_opt with
       | None ->
         pair
           (lift_expr (inner ctx) e1)
           (lift_expr (inner ctx) e2)
           (fun e1' e2' -> ExpBranch (e1', e2', None))
       | Some e3 ->
         triple
           (lift_expr (inner ctx) e1)
           (lift_expr (inner ctx) e2)
           (lift_expr (inner ctx) e3)
           (fun e1' e2' e3' -> ExpBranch (e1', e2', Some e3')))
    | ExpConstruct (id, Some e) ->
      let* res = lift_expr (inner ctx) e in
      return { res with expr = ExpConstruct (id, Some res.expr) }
    | ExpTypeAnnotation (e, typ) ->
      let* res = lift_expr (inner ctx) e in
      return { res with expr = ExpTypeAnnotation (res.expr, typ) }
    | ExpBinOper (op, e1, e2) ->
      pair
        (lift_expr (inner ctx) e1)
        (lift_expr (inner ctx) e2)
        (fun e1' e2' -> ExpBinOper (op, e1', e2'))
    | ExpUnarOper (op, e) ->
      let* res = lift_expr (inner ctx) e in
      return { res with expr = ExpUnarOper (op, res.expr) }
    | ExpTuple (e1, e2, rest) ->
      let* first = lift_expr (inner ctx) e1 in
      let* second = lift_expr (inner ctx) e2 in
      let* rest_structures, rest_exprs = list rest (lift_expr (inner ctx)) in
      return
        { structures = first.structures @ second.structures @ rest_structures
        ; expr = ExpTuple (first.expr, second.expr, rest_exprs)
        }
    | ExpList es ->
      let* elem_structures, lifted_elems = list es (lift_expr (inner ctx)) in
      return { structures = elem_structures; expr = ExpList lifted_elems }
    | ExpOption None -> return { structures = []; expr = ExpOption None }
    | ExpOption (Some e) ->
      let* res = lift_expr (inner ctx) e in
      return { res with expr = ExpOption (Some res.expr) }

  and lift_binds (ctx : context) binds : (structure list * (pattern * expr) list) t =
    fold_binds ctx binds (fun ctx _ e -> lift_expr ctx e)

  and lift_binds_with_pattern_scope (ctx : context) binds
    : (structure list * (pattern * expr) list) t
    =
    fold_binds ctx binds (fun ctx p e ->
      let ctx_binding =
        { ctx with renames = without_bindings ctx.renames (names_in_pattern p) }
      in
      lift_expr ctx_binding e)
  ;;

  let lift_structure : structure -> structure list t = function
    | SEval e ->
      let toplevel = { renames = initial_renames; at_toplevel = true } in
      let* res = lift_expr toplevel e in
      return (res.structures @ [ SEval res.expr ])
    | SValue (is_rec, bind, more) ->
      let toplevel = { renames = initial_renames; at_toplevel = true } in
      let* inner_structures, lifted_binds = lift_binds toplevel (bind :: more) in
      (match lifted_binds with
       | first :: rest -> return (inner_structures @ [ SValue (is_rec, first, rest) ])
       | [] -> fail SValueEmptyBinding)
  ;;

  let run_program (program : program) (naming_init : N.t)
    : (structure list * N.t, error) Result.t
    =
    let m =
      List.fold_left
        (fun acc item ->
           let* rev_structure_lists = acc in
           let* struct_structures = lift_structure item in
           return (struct_structures :: rev_structure_lists))
        (return [])
        program
    in
    match m naming_init with
    | Ok (rev_structure_lists, st_final) ->
      Ok (List.concat (List.rev rev_structure_lists), st_final)
    | Error e -> Error e
  ;;
end

module Transform = Make (Default_naming)

let lambda_lifting_result (program : Frontend.Ast.program)
  : (structure list, error) Result.t
  =
  match Transform.run_program program Default_naming.init with
  | Ok (lst, _) -> Ok lst
  | Error e -> Error e
;;
