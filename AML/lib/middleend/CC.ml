(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf_types
module SS = Set.Make (String)
module SM = Map.Make (String)

type cc_state =
  { temps : int
  ; bound : SS.t
  }

module CCState = struct
  type 'a t = cc_state -> ('a, string) result * cc_state

  let return x st = Ok x, st
  let error msg st = Error msg, st

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun st ->
    match m st with
    | Error msg, new_state -> Error msg, new_state
    | Ok a, new_state -> f a new_state
  ;;

  let ( let* ) = bind
  let get st = Ok st, st
  let put st _ = Ok (), st

  let rec map_m f = function
    | [] -> return []
    | x :: xs ->
      let* y = f x in
      let* ys = map_m f xs in
      return (y :: ys)
  ;;

  let run m = m { temps = 0; bound = SS.empty }
end

open CCState

let fresh_name (base : string) : string CCState.t =
  let* st = get in
  let new_id = st.temps in
  let* () = put { st with temps = new_id + 1 } in
  return (Printf.sprintf "%s_cc_%d" base new_id)
;;

let free_vars_imm = function
  | ImmNum _ -> SS.empty
  | ImmId id -> SS.singleton id
;;

let rec free_vars_cexpr = function
  | CImm imm -> free_vars_imm imm
  | CBinop (_, i1, i2) -> SS.union (free_vars_imm i1) (free_vars_imm i2)
  | CApp (f, args) ->
    List.fold_left (fun acc a -> SS.union acc (free_vars_imm a)) (free_vars_imm f) args
  | CIte (i, thn, els) ->
    SS.union (free_vars_imm i) (SS.union (free_vars_aexpr thn) (free_vars_aexpr els))
  | CFun (arg, body) -> SS.remove arg (free_vars_aexpr body)

and free_vars_aexpr = function
  | ACE cexpr -> free_vars_cexpr cexpr
  | ALet (Nonrecursive, name, rhs, body) ->
    let rhs_fvs = free_vars_cexpr rhs in
    let body_fvs = SS.remove name (free_vars_aexpr body) in
    SS.union rhs_fvs body_fvs
  | ALet (Recursive, name, rhs, body) ->
    let rhs_fvs = SS.remove name (free_vars_cexpr rhs) in
    let body_fvs = SS.remove name (free_vars_aexpr body) in
    SS.union rhs_fvs body_fvs
;;

(* helper to run a computation `comp` in a new lexical scope *)
let with_new_scope (new_bound : SS.t) (comp : 'a CCState.t) =
  let* old_st = get in
  let* () = put { old_st with bound = new_bound } in
  let* result = comp in
  (* restore the old bound set, keep updated temps counter *)
  let* state_after_comp = get in
  let* () = put { old_st with temps = state_after_comp.temps } in
  return result
;;

(* helper for `with_new_scope` to add new name to the bound set *)
let with_binding (name : string) (comp : 'a CCState.t) =
  let* st = get in
  let new_bound = SS.add name st.bound in
  with_new_scope new_bound comp
;;

(* build a curried function from a list of parameters. e.g., [p1; p2] body -> CFun(p1, ACE(CFun(p2, body))) *)
let rec mk_fun_cexpr (params : string list) (body : aexpr) : cexpr =
  match params with
  | [ p ] -> CFun (p, body)
  | p :: ps -> CFun (p, ACE (mk_fun_cexpr ps body))
  | _ -> raise (Invalid_argument "unreachable: mk_fun_cexpr with empty params")
;;

let rec cc_aexpr = function
  | ACE cexpr -> cc_cexpr_to_aexpr cexpr
  | ALet (rec_flag, name, rhs, body) ->
    (match rhs with
     | CFun (arg, f_body) ->
       let* st = get in
       let fvs = free_vars_cexpr (CFun (arg, f_body)) in
       let captured = SS.elements (SS.inter fvs st.bound) in
       (match captured with
        | [] ->
          (* closed function case *)
          let inner_bound =
            match rec_flag with
            | Recursive -> SS.add arg (SS.add name st.bound)
            | Nonrecursive -> SS.add arg st.bound
          in
          let* converted_fun_body = with_new_scope inner_bound (cc_aexpr f_body) in
          let* converted_let_body = with_binding name (cc_aexpr body) in
          return
            (ALet (rec_flag, name, CFun (arg, converted_fun_body), converted_let_body))
        | _ ->
          (* transform current let into let for the helper function, followed by a let for the closure *)
          let* helper_name = fresh_name name in
          let helper_params =
            match rec_flag with
            | Recursive -> name :: (captured @ [ arg ])
            | Nonrecursive -> captured @ [ arg ]
          in
          let inner_bound =
            List.fold_left (fun acc p -> SS.add p acc) st.bound helper_params
          in
          let* converted_helper_body = with_new_scope inner_bound (cc_aexpr f_body) in
          let lifted_helper_fun = mk_fun_cexpr helper_params converted_helper_body in
          let closure_args =
            match rec_flag with
            | Recursive -> ImmId name :: List.map (fun x -> ImmId x) captured
            | Nonrecursive -> List.map (fun x -> ImmId x) captured
          in
          let closure = CApp (ImmId helper_name, closure_args) in
          let* converted_let_body = with_binding name (cc_aexpr body) in
          (* return new structure:
          * let helper = ... in
          * let [rec] name = closure in
          * ... body ...
          *)
          return
            (ALet
               ( Nonrecursive
               , helper_name
               , lifted_helper_fun
               , ALet (rec_flag, name, closure, converted_let_body) )))
     | _ ->
       (* non-function let. e.g., let x = 5 *)
       let* converted_rhs = cc_cexpr rhs in
       let* converted_body = with_binding name (cc_aexpr body) in
       return (ALet (rec_flag, name, converted_rhs, converted_body)))

(* cc for cexpr in tail position *)
and cc_cexpr_to_aexpr = function
  | CFun (arg, body) ->
    (* (fun x -> ...) *)
    let* st = get in
    let fvs = free_vars_cexpr (CFun (arg, body)) in
    let captured = SS.elements (SS.inter fvs st.bound) in
    (match captured with
     | [] ->
       let inner_bound = SS.add arg st.bound in
       let* converted_body = with_new_scope inner_bound (cc_aexpr body) in
       return (ACE (CFun (arg, converted_body)))
     | _ ->
       (* (fun x -> ...)
       * becomes:
       * let helper_f = fun cap1 cap2 ... x -> ... in
       * helper_f (cap1_val, cap2_val, ...)
       *)
       let* helper_name = fresh_name "f" in
       let helper_params = captured @ [ arg ] in
       let inner_bound =
         List.fold_left (fun acc p -> SS.add p acc) st.bound (captured @ [ arg ])
       in
       let* converted_helper_body = with_new_scope inner_bound (cc_aexpr body) in
       let lifted_helper_fun = mk_fun_cexpr helper_params converted_helper_body in
       let closure_creation =
         CApp (ImmId helper_name, List.map (fun x -> ImmId x) captured)
       in
       return (ALet (Nonrecursive, helper_name, lifted_helper_fun, ACE closure_creation)))
  | CApp (f, args) -> return (ACE (CApp (f, args)))
  | CImm imm -> return (ACE (CImm imm))
  | CBinop (op, i1, i2) -> return (ACE (CBinop (op, i1, i2)))
  | CIte (i, thn, els) ->
    let* converted_thn = cc_aexpr thn in
    let* converted_els = cc_aexpr els in
    return (ACE (CIte (i, converted_thn, converted_els)))

(* cc for cexpr in non-tail position *)
and cc_cexpr = function
  | CApp (f, args) -> return (CApp (f, args))
  | CImm imm -> return (CImm imm)
  | CBinop (op, i1, i2) -> return (CBinop (op, i1, i2))
  | CIte (i, thn, els) ->
    let* converted_thn = cc_aexpr thn in
    let* converted_els = cc_aexpr els in
    return (CIte (i, converted_thn, converted_els))
  | CFun _ -> error "unreachable: cc_cexpr should not encounter CFun"
;;

let cc_str_item = function
  | AStr_eval aexpr ->
    let* converted_aexpr = cc_aexpr aexpr in
    return [ AStr_eval converted_aexpr ]
  | AStr_value (rec_flag, name, (ACE (CFun (arg, f_body)) as func)) ->
    let* st = get in
    let fvs = free_vars_aexpr func in
    let captured = SS.elements (SS.inter fvs st.bound) in
    (match captured with
     | [] ->
       let inner_bound =
         match rec_flag with
         | Recursive -> SS.add arg (SS.add name st.bound)
         | Nonrecursive -> SS.add arg st.bound
       in
       let* converted_body = with_new_scope inner_bound (cc_aexpr f_body) in
       let* state_after_body = get in
       let new_toplevel_bound = SS.add name st.bound in
       let* () = put { state_after_body with bound = new_toplevel_bound } in
       return [ AStr_value (rec_flag, name, ACE (CFun (arg, converted_body))) ]
     | _ ->
       let* helper_name = fresh_name name in
       let helper_params =
         match rec_flag with
         | Recursive -> name :: (captured @ [ arg ])
         | Nonrecursive -> captured @ [ arg ]
       in
       let inner_bound =
         List.fold_left (fun acc p -> SS.add p acc) st.bound helper_params
       in
       let* converted_helper_body = with_new_scope inner_bound (cc_aexpr f_body) in
       let lifted_helper_fun = mk_fun_cexpr helper_params converted_helper_body in
       let helper_item = AStr_value (Nonrecursive, helper_name, ACE lifted_helper_fun) in
       let closure_item =
         match rec_flag with
         | Nonrecursive ->
           let clos = CApp (ImmId helper_name, List.map (fun x -> ImmId x) captured) in
           AStr_value (Nonrecursive, name, ACE clos)
         | Recursive ->
           (*
              * recursive 'name' is now a wrapper that calls helper with the captured environment and original argument
           *
           * let rec name = fun arg ->
           * helper_name (name, cap1, cap2, ..., arg)
           *)
           let clos_args = ImmId name :: List.map (fun x -> ImmId x) captured in
           let wrapper =
             ACE (CFun (arg, ACE (CApp (ImmId helper_name, clos_args @ [ ImmId arg ]))))
           in
           AStr_value (Recursive, name, wrapper)
       in
       let* state_after_body = get in
       let new_toplevel_bound = SS.add helper_name (SS.add name st.bound) in
       let* () = put { state_after_body with bound = new_toplevel_bound } in
       return [ helper_item; closure_item ])
  | AStr_value (rec_flag, name, aexpr) ->
    let* st = get in
    let inner_bound =
      match rec_flag with
      | Recursive -> SS.add name st.bound
      | Nonrecursive -> st.bound
    in
    let* converted_aexpr = with_new_scope inner_bound (cc_aexpr aexpr) in
    let* state_after_body = get in
    let new_toplevel_bound = SS.add name st.bound in
    let* () = put { state_after_body with bound = new_toplevel_bound } in
    return [ AStr_value (rec_flag, name, converted_aexpr) ]
;;

let cc_program (prog : aprogram) : aprogram CCState.t =
  let* list_of_converted_items = CCState.map_m cc_str_item prog in
  return (List.concat list_of_converted_items)
;;

let cc_transform (prog : aprogram) : (aprogram, string) result =
  match CCState.run (cc_program prog) with
  | Ok converted_program, _ -> Ok converted_program
  | Error error_message, _ -> Error error_message
;;
