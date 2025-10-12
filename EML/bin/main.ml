(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib.Frontend.Parser
open EML_lib.Riskv.Codogen

type opts =
  { mutable input_file : string
  ; mutable output_file : string
  }


let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  String.trim s

let write_file filename content =
  let ch = open_out filename in
  output_string ch content;
  close_out ch

let compiler input_file output_file =
  let input = read_file input_file in
  match parse input with
  | Result.Error e -> 
      Printf.eprintf "Parsing error: %s\n" e;
      exit 1
  | Result.Ok program ->
      let riscv_code = compile_to_riscv program in
      write_file output_file riscv_code

let () =
  let opts = { input_file = ""; output_file = "a.s" } in
  let open Stdlib.Arg in
  let speclist =
    [ "-fromfile", String (fun filename -> opts.input_file <- filename), "Input file name"
    ; "-o", String (fun filename -> opts.output_file <- filename), "Output file name"
    ]
  in
  let anon_func filename =
    opts.input_file <- filename
  in
  let usage_msg = "Mini-ml to riscv compiler" in
  parse speclist anon_func usage_msg;
  if opts.input_file = "" then (
    Printf.eprintf "Error: Input file must be specified\n";
    exit 1
  );
  compiler opts.input_file opts.output_file