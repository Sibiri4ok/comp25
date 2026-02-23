(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib.Frontend.Parser
open EML_lib.Frontend.Inferencer
open EML_lib.Middleend.Anf
open EML_lib.Backend.Ricsv.Runner
open Stdlib

let read_stdin () =
  let buf = Buffer.create 4096 in
  let rec loop () =
    match input_char stdin with
    | c -> Buffer.add_char buf c; loop ()
    | exception End_of_file -> Buffer.contents buf
  in
  loop ()

let compile_to_riscv input =
  match parse input with
  | Error e ->
    Printf.eprintf "Parse error: %s\n" e;
    exit 1
  | Ok ast -> (
    match run_infer ast with
    | Error _ ->
      Printf.eprintf "Type error (inference failed)\n";
      exit 1
    | Ok _ -> (
      match anf_program ast with
      | Error e ->
        Printf.eprintf "ANF error: %s\n" e;
        exit 1
      | Ok anf ->
        gen_program Stdlib.Format.std_formatter anf))

let () =
  let input =
    match Array.length Sys.argv with
    | 1 -> read_stdin ()
    | 2 ->
      let path = Sys.argv.(1) in
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    | _ ->
      Printf.eprintf "Usage: %s [file.ml]\n" Sys.argv.(0);
      exit 1
  in
  compile_to_riscv input
