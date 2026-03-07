(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Stdio
open EML_lib
open Frontend

type opts =
  { input_file : string option
  ; output_file : string option
  ; enable_gc : bool
  }

let default_opts = { input_file = None; output_file = None; enable_gc = false }

type env = Inferencer.TypeEnv.t

let report_parse_error oc s =
  Out_channel.output_string oc (Format.asprintf "Parsing error: %s\n" s)
;;

let report_infer_error oc e =
  Out_channel.output_string
    oc
    (Format.asprintf "Inferencer error: %a\n" Inferencer.pp_error e)
;;

let with_frontend text env oc f_success : (env, unit) Result.t =
  match Frontend.Runner.run text env with
  | Error (Frontend.Runner.Parse s) ->
    report_parse_error oc s;
    Error ()
  | Error (Frontend.Runner.Infer e) ->
    report_infer_error oc e;
    Error ()
  | Ok (ast, env', out_list) -> f_success ast env' out_list
;;

let with_middleend ast _env' f : (env, unit) Result.t =
  match Middleend.Runner.run ast with
  | Error e_mid ->
    Format.eprintf "Middleend error: %a\n%!" Middleend.Runner.pp_error e_mid;
    Error ()
  | Ok anf_ast -> f anf_ast
;;

let run_compile ~enable_gc text env oc : (env, unit) Result.t =
  with_frontend text env oc (fun ast env' _out_list ->
    with_middleend ast env' (fun anf_ast ->
      let ppf = Format.formatter_of_out_channel oc in
      let res = Backend.Ricsv.Runner.gen_program ~enable_gc ppf anf_ast in
      match res with
      | Ok () -> Ok env'
      | Error msg ->
        Format.eprintf "Codegen error: %s\n%!" msg;
        Error ()))
;;

(* ------------------------------------------------------------------------- *)
(* Compiler entry point                                                      *)
(* ------------------------------------------------------------------------- *)

let compiler opts : (unit, unit) Result.t =
  let run text env oc = run_compile ~enable_gc:opts.enable_gc text env oc in
  let env0 =
    if opts.enable_gc then Inferencer.TypeEnv.env_with_gc else Inferencer.TypeEnv.initial_env
  in
  let with_output f =
    match opts.output_file with
    | Some path -> Out_channel.with_file path ~f
    | None -> f Out_channel.stdout
  in
  let input =
    match opts.input_file with
    | Some path -> In_channel.read_all path |> String.trim
    | None -> In_channel.input_all stdin |> String.trim
  in
  match with_output (fun oc -> run input env0 oc) with
  | Ok _env -> Ok ()
  | Error () -> Error ()
;;

(* ------------------------------------------------------------------------- *)
(* CLI                                                                       *)
(* ------------------------------------------------------------------------- *)

let parse_args () : (opts, unit) Result.t =
  let input_file = ref default_opts.input_file in
  let output_file = ref default_opts.output_file in
  let enable_gc = ref default_opts.enable_gc in
  let positional_seen = ref false in
  let open Arg in
  let spec =
    [ "-fromfile", String (fun s -> input_file := Some s), " <file> Read source from file"
    ; "-o", String (fun s -> output_file := Some s), " <file> Write output to file"
    ; "-gc", Set enable_gc, " Enable GC runtime support"
    ]
  in
  parse spec (fun _ -> positional_seen := true) "Compiler for custom language";
  if !positional_seen
  then Error ()
  else Ok { input_file = !input_file; output_file = !output_file; enable_gc = !enable_gc }
;;

let () =
  match parse_args () with
  | Error () ->
    Format.eprintf "Positional arguments are not supported\n";
    exit 1
  | Ok opts ->
    (match compiler opts with
     | Error () -> exit 1
     | Ok () -> ())
;;
