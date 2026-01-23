(* * Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Emlib.Frontend
open Emlib.Middleend
open Emlib.Backend.Rv64
open Format

let compile_file filename =
  let filename =
    if Filename.is_relative filename
    then Filename.concat (Sys.getcwd ()) filename
    else filename
  in
  let input =
    let ic = open_in filename in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    s
  in
  match Parser.parse input with
  | Error e ->
    eprintf "Parsing error: %s\n" e;
    exit 1
  | Ok ast ->
    (match Anf.anf_program ast with
     | Error e ->
       eprintf "ANF error: %s\n" e;
       exit 1
     | Ok anf ->
       let asm = Codegen.gen_program anf in
       printf "%a\n" Asm_pp.pp_program asm)
;;

let () =
  if Array.length Sys.argv <> 2
  then (
    eprintf "usage: dune exec EML -- <file.ml>\n";
    exit 1);
  compile_file Sys.argv.(1)
;;
