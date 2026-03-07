(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Analysis

let gen_program ~enable_gc ppf (program : anf_program) =
  let analysis = analyze program in
  Generator.gen_program ~enable_gc ppf analysis
;;
