(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm

module type NAMING = sig
  type t

  val init : t
  val fresh_blocks : t -> (string * string * string) * t
end

module Default_naming : NAMING = struct
  type t = int

  let init = 0

  let fresh_blocks n =
    let then_name = "then_" ^ Int.to_string n in
    let else_name = "else_" ^ Int.to_string n in
    let merge_name = "merge_" ^ Int.to_string n in
    (then_name, else_name, merge_name), n + 1
  ;;
end

module Make (N : NAMING) = struct
  type state =
    { value_env : (string, llvalue, Base.String.comparator_witness) Base.Map.t
    ; type_env : (string, lltype, Base.String.comparator_witness) Base.Map.t
    ; current_module : llmodule
    ; gc_allocas : (string, llvalue, Base.String.comparator_witness) Base.Map.t option
    ; gc_entry_block : llbasicblock option
    ; naming_state : N.t
    ; resolve : (int -> string -> (string * int) option) option
    ; current_func_index : int
    }

  type 'a t = state -> ('a * state, string) Result.t

  let return x state = Ok (x, state)

  let bind m f state =
    match m state with
    | Ok (x, state') -> f x state'
    | Error err -> Error err
  ;;

  let ( let* ) = bind
  let get state = Ok (state, state)
  let put state _ = Ok ((), state)

  let modify f state =
    match get state with
    | Ok (current_state, _) -> put (f current_state) state
    | Error err -> Error err
  ;;

  let fail err = fun _ -> Error err
  let map_find_opt (map : (string, 'a, _) Base.Map.t) key = Base.Map.find map key
  let find_value_opt name state = Ok (Base.Map.find state.value_env name, state)
  let find_type_opt name state = Ok (Base.Map.find state.type_env name, state)

  let resolve_key state name =
    match state.resolve with
    | None -> name
    | Some resolver ->
      (match resolver state.current_func_index name with
       | Some (asm_name, _) -> asm_name
       | None -> name)
  ;;

  let resolved_find_value_opt name state =
    let resolved_key = resolve_key state name in
    Ok (Base.Map.find state.value_env resolved_key, state)
  ;;

  let resolved_find_type_opt name state =
    let resolved_key = resolve_key state name in
    Ok (Base.Map.find state.type_env resolved_key, state)
  ;;

  let set_value name value =
    modify (fun state ->
      { state with value_env = Base.Map.set state.value_env ~key:name ~data:value })
  ;;

  let set_type name lltype =
    modify (fun state ->
      { state with type_env = Base.Map.set state.type_env ~key:name ~data:lltype })
  ;;

  let remove_value name =
    modify (fun state -> { state with value_env = Base.Map.remove state.value_env name })
  ;;

  let get_gc_allocas state = Ok (state.gc_allocas, state)

  let set_gc_allocas allocas_map =
    modify (fun state -> { state with gc_allocas = allocas_map })
  ;;

  let get_gc_entry_block state = Ok (state.gc_entry_block, state)

  let set_gc_entry_block block =
    modify (fun state -> { state with gc_entry_block = block })
  ;;

  let fresh_blocks =
    let* state = get in
    let triple, next = N.fresh_blocks state.naming_state in
    let* () = put { state with naming_state = next } in
    return triple
  ;;

  let run m = m
end

include Make (Default_naming)
