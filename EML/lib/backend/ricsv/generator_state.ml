(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Frontend.Ast
open Architecture.Riscv_backend

type env = (ident, location, String.comparator_witness) Map.t

type state =
  { frame_offset : int
  ; fresh_id : int
  ; arity_map : (ident, int, String.comparator_witness) Map.t
  ; env : env
  ; instr_buffer : instr list
  }

type 'a t = state -> ('a * state, string) Result.t

let return x st = Ok (x, st)
let fail e = fun _ -> Error e

let bind m f =
  fun state ->
  match m state with
  | Ok (x, st') -> f x st'
  | Error e -> Error e
;;

let ( let* ) = bind
let get st = Ok (st, st)
let put st = fun _ -> Ok ((), st)

let modify f =
  let* st = get in
  put (f st)
;;

let modify_env f = modify (fun st -> { st with env = f st.env })

let get_env =
  let* st = get in
  return st.env
;;

let set_env env = modify (fun st -> { st with env })

let fresh =
  let modify_fresh_id f = modify (fun st -> { st with fresh_id = f st.fresh_id }) in
  let* st = get in
  let* () = modify_fresh_id Int.succ in
  return st.fresh_id
;;

let run m init = m init

let append (items : instr list) =
  let modify_instr_buffer f =
    modify (fun st -> { st with instr_buffer = f st.instr_buffer })
  in
  if List.is_empty items
  then return ()
  else
    modify_instr_buffer (fun l ->
      List.fold_left items ~init:l ~f:(fun acc it -> it :: acc))
;;
