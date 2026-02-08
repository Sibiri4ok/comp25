(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Anf_types

type ll_state =
  { temps : int
  ; lifted : astructure_item list
  }

module LLState = struct
  type 'a t = ll_state -> ('a, string) Result.t * ll_state

  let return x st = Ok x, st

  let bind m f st =
    match m st with
    | Error e, new_state -> Error e, new_state
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

  let run m = m { temps = 0; lifted = [] }
end

open LLState

let fresh_lam_name =
  let* st = get in
  let name = Printf.sprintf "llf_%d" st.temps in
  let* () = put { st with temps = st.temps + 1 } in
  return name
;;

let rec collect_cfun acc = function
  | ACE (CFun (p, body')) -> collect_cfun (p :: acc) body'
  | body -> List.rev acc, body
;;

let rec build_curried params body =
  match params with
  | [] -> body
  | p :: ps -> ACE (CFun (p, build_curried ps body))
;;

let rec ll_cexpr = function
  | CImm i -> return (CImm i)
  | CBinop (op, a, b) -> return (CBinop (op, a, b))
  | CApp (f, args) -> return (CApp (f, args))
  | CIte (c, th, el) ->
    let* lifted_then = ll_aexpr th in
    let* lifted_else = ll_aexpr el in
    return (CIte (c, lifted_then, lifted_else))
  | CFun (p, original_body) ->
    let params, unwrapped_body = collect_cfun [ p ] original_body in
    let* lifted_body = ll_aexpr unwrapped_body in
    let* lifted_fun_name = fresh_lam_name in
    let new_top_level_fun = build_curried params lifted_body in
    let* st = get in
    let new_lifted_item = AStr_value (Nonrecursive, lifted_fun_name, new_top_level_fun) in
    let* () = put { st with lifted = new_lifted_item :: st.lifted } in
    return (CImm (ImmId lifted_fun_name))

and ll_aexpr = function
  (* (let x = y in x) ==> y *)
  | ALet (Nonrecursive, x, CImm (ImmId y), ACE (CImm (ImmId x'))) when String.equal x x'
    -> return (ACE (CImm (ImmId y)))
  (* (let x = f in x(args)) ==> f(args) *)
  | ALet (Nonrecursive, x, CImm (ImmId f), ACE (CApp (ImmId callee, args)))
    when String.equal x callee -> return (ACE (CApp (ImmId f, args)))
  | ACE c ->
    let* lifted_cexpr = ll_cexpr c in
    return (ACE lifted_cexpr)
  | ALet (rf, name, rhs, body) ->
    let* lifted_rhs = ll_cexpr rhs in
    let* lifted_body = ll_aexpr body in
    return (ALet (rf, name, lifted_rhs, lifted_body))
;;

let ll_program (prog : aprogram) : aprogram LLState.t =
  let* processed_items =
    map_m
      (function
        | AStr_value (rf, name, a) ->
          let* lifted_aexpr = ll_aexpr a in
          return (AStr_value (rf, name, lifted_aexpr))
        | AStr_eval a ->
          let* lifted_aexpr = ll_aexpr a in
          return (AStr_eval lifted_aexpr))
      prog
  in
  let* st = get in
  return (List.rev st.lifted @ processed_items)
;;

let ll_transform (program : aprogram) : (aprogram, string) Result.t =
  match LLState.run (ll_program program) with
  | Ok lifted_program, _ -> Ok lifted_program
  | Error error_message, _ -> Error error_message
;;
