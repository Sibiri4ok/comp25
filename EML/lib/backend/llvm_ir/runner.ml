(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf

let gen_program ~enable_gc ppf (program : anf_program) : (unit, string) Result.t =
  let temp_ll_path = Filename.temp_file "eml_llvm" ".ll" in
  let remove_temp_file_if_exists () =
    if Sys.file_exists temp_ll_path then Sys.remove temp_ll_path
  in
  match Generator.gen_program ~output_file:temp_ll_path ~enable_gc program with
  | Error err ->
    remove_temp_file_if_exists ();
    Error err
  | Ok () ->
    let content = In_channel.with_open_text temp_ll_path In_channel.input_all in
    remove_temp_file_if_exists ();
    Format.fprintf ppf "%s" content;
    Ok ()
;;
