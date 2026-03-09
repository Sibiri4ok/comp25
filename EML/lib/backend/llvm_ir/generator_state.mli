(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm

module type NAMING = sig
  type t

  val init : t
  val fresh_blocks : t -> (string * string * string) * t
end

module Default_naming : NAMING

module Make (N : NAMING) : sig
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

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val get : state t
  val put : state -> unit t
  val modify : (state -> state) -> unit t
  val fail : string -> 'a t
  val map_find_opt : (string, 'a, 'cmp) Base.Map.t -> string -> 'a option
  val find_value_opt : string -> llvalue option t
  val find_type_opt : string -> lltype option t
  val resolved_find_value_opt : string -> llvalue option t
  val resolved_find_type_opt : string -> lltype option t
  val set_value : string -> llvalue -> unit t
  val set_type : string -> lltype -> unit t
  val remove_value : string -> unit t

  val get_gc_allocas
    : (string, llvalue, Base.String.comparator_witness) Base.Map.t option t

  val set_gc_allocas
    :  (string, llvalue, Base.String.comparator_witness) Base.Map.t option
    -> unit t

  val get_gc_entry_block : llbasicblock option t
  val set_gc_entry_block : llbasicblock option -> unit t
  val fresh_blocks : (string * string * string) t
  val run : 'a t -> state -> ('a * state, string) Result.t
end

include module type of Make (Default_naming)
