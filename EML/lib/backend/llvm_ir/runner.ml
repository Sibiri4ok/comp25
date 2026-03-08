(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf

let gen_program ~enable_gc ppf (program : anf_program) : (unit, string) Result.t =
  let temp_ll_path = Filename.temp_file "eml_llvm" ".ll" in
  match Generator.gen_program ~output_file:temp_ll_path ~enable_gc program with
  | Error err ->
    (try Sys.remove temp_ll_path with
     | _ -> ());
    Error err
  | Ok () ->
    let content = In_channel.with_open_text temp_ll_path In_channel.input_all in
    Sys.remove temp_ll_path;
    Format.fprintf ppf "%s" content;
    Ok ()
;;
