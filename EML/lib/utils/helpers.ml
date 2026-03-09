(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Frontend
open Ast

let is_simple_pattern = function
  | PatVariable _ | PatAny | PatUnit -> true
  | _ -> false
;;

let is_tuple_pattern = function
  | PatTuple (_, _, _) -> true
  | _ -> false
;;

let rec extract_tuple_pattern_idents acc = function
  | PatVariable x -> x :: acc
  | PatTuple (p1, p2, rest) ->
    let acc' = extract_tuple_pattern_idents acc p1 in
    let acc'' = extract_tuple_pattern_idents acc' p2 in
    List.fold_left rest ~f:extract_tuple_pattern_idents ~init:acc''
  | PatAny -> "_" :: acc
  | _ -> acc
;;

let pattern_to_ident = function
  | PatVariable x -> Some x
  | _ -> None
;;
