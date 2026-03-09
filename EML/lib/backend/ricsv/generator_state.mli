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

module Default_naming : NAMING

module Make (N : NAMING) : sig
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

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val get : state t
  val put : state -> unit t
  val modify : (state -> state) -> unit t
  val modify_env : (env -> env) -> unit t
  val get_env : env t
  val set_env : env -> unit t
  val fresh_partial : string t
  val fresh_branch : (string * string) t
  val run : 'a t -> state -> ('a * state, string) Result.t
  val append : instr list -> unit t
end

include module type of Make (Default_naming)
