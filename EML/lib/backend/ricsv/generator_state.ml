(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Architecture
open Riscv_backend

module type NAMING = sig
  type t

  val init : t
  val fresh_partial : t -> string * t
  val fresh_branch : t -> (string * string) * t
end

module Default_naming : NAMING = struct
  type t = int

  let init = 0
  let fresh_partial n = "part_" ^ string_of_int n, n + 1
  let fresh_branch n = ("else_" ^ string_of_int n, "end_" ^ string_of_int n), n + 1
end

module Make (N : NAMING) = struct
  type env = (string, location, Base.String.comparator_witness) Base.Map.t

  type state =
    { frame_offset : int
    ; naming_state : N.t
    ; arity_map : (string, int, Base.String.comparator_witness) Base.Map.t
    ; env : env
    ; instr_buffer : instr list
    ; current_func_index : int
    ; symbol_resolve : int -> string -> (string * int) option
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

  let fresh_partial =
    let* st = get in
    let name, next = N.fresh_partial st.naming_state in
    let* () = put { st with naming_state = next } in
    return name
  ;;

  let fresh_branch =
    let* st = get in
    let pair, next = N.fresh_branch st.naming_state in
    let* () = put { st with naming_state = next } in
    return pair
  ;;

  let run m = m

  let append (items : instr list) =
    let modify_instr_buffer f =
      modify (fun st -> { st with instr_buffer = f st.instr_buffer })
    in
    match items with
    | [] -> return ()
    | _ ->
      modify_instr_buffer (fun buffer ->
        List.fold_left (fun acc instruction -> instruction :: acc) buffer items)
  ;;
end

include Make (Default_naming)
