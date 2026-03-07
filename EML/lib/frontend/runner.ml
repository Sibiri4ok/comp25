(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format
open Inferencer

type error =
  | Parse of string
  | Infer of Inferencer.error

let pp_error ppf = function
  | Parse s -> fprintf ppf "Parse error: %s" s
  | Infer e -> fprintf ppf "Inference error: %a" Inferencer.pp_error e
;;

let parse (text : string) : (program, string) Result.t = Parser.parse text

let run (text : string) (env : TypeEnv.t)
  : (program * TypeEnv.t * (ident option * ty) list, error) Result.t
  =
  match Parser.parse text with
  | Error s -> Error (Parse s)
  | Ok ast ->
    (match Inferencer.ResultMonad.run (infer_structure env ast) with
     | Error e -> Error (Infer e)
     | Ok (env', out_list) -> Ok (ast, env', out_list))
;;
