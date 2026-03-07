(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Frontend.Ast
open Cc
open Ll
open Anf

type error =
  | Closure of Cc.error
  | Lifting of Ll.error
  | Anf of string

let pp_error ppf = function
  | Closure e -> fprintf ppf "closure conversion: %a" Cc.pp_error e
  | Lifting e -> fprintf ppf "lambda lifting: %a" Ll.pp_error e
  | Anf s -> fprintf ppf "ANF: %s" s
;;

let run (program : program) : (anf_program, error) Result.t =
  let ( >>= ) = Result.bind in
  closure_conversion_result program
  |> Result.map_error (fun e -> Closure e)
  >>= fun cc_ast ->
  lambda_lifting_result cc_ast
  |> Result.map_error (fun e -> Lifting e)
  >>= fun ll_ast -> anf_program ll_ast |> Result.map_error (fun e -> Anf e)
;;
