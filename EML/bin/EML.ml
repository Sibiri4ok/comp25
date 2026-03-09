(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Stdio
open EML_lib
open Middleend

type backend =
  | Ricsv
  | Llvm

type opts =
  { input_file : string option
  ; output_file : string option
  ; enable_gc : bool
  ; backend : backend
  ; infer_only : bool
  }

let default_opts =
  { input_file = None
  ; output_file = None
  ; enable_gc = false
  ; backend = Ricsv
  ; infer_only = false
  }
;;

type env = Inferencer.TypeEnv.t

let report_parse_error oc s =
  Out_channel.output_string oc (Format.asprintf "Parsing error: %s\n" s)
;;

let report_infer_error oc e =
  Out_channel.output_string
    oc
    (Format.asprintf "Inferencer error: %a\n" Inferencer.pp_error e)
;;

let with_frontend text oc f_success : (env, unit) Result.t =
  match Frontend.Runner.run text with
  | Error (Frontend.Runner.Parse s) ->
    report_parse_error oc s;
    Error ()
  | Ok ast -> f_success ast
;;

let with_middleend ast env f : (env, unit) Result.t =
  match Middleend.Runner.run ast env with
  | Error e_mid ->
    Format.eprintf "Middleend error: %a\n%!" Middleend.Runner.pp_error e_mid;
    Error ()
  | Ok (anf_ast, env') -> f anf_ast env'
;;

let run_compile text env oc ~backend ~enable_gc : (env, unit) Result.t =
  with_frontend text oc (fun ast ->
    with_middleend ast env (fun anf_ast env' ->
      let ppf = Format.formatter_of_out_channel oc in
      let res =
        match backend with
        | Ricsv -> Backend.Ricsv.Runner.gen_program ~enable_gc ppf anf_ast
        | Llvm -> Backend.Llvm_ir.Runner.gen_program ~enable_gc ppf anf_ast
      in
      match res with
      | Ok () -> Ok env'
      | Error msg ->
        Format.eprintf "Codegen error: %s\n%!" msg;
        Error ()))
;;

let run_infer_only text env oc : (env, unit) Result.t =
  match Frontend.Parser.parse text with
  | Error s ->
    report_parse_error oc s;
    Error ()
  | Ok ast ->
    (match Inferencer.ResultMonad.run (Inferencer.infer_structure env ast) with
     | Error e ->
       report_infer_error oc e;
       Error ()
     | Ok (_subst, env') ->
       let filtered_env =
         Base.Map.filter_keys env' ~f:(fun key -> not (Base.Map.mem env key))
       in
       Base.Map.iteri filtered_env ~f:(fun ~key ~data ->
         match data with
         | Inferencer.Scheme.Scheme (_, ty) ->
           Out_channel.output_string
             oc
             (Format.asprintf "val %s: %a\n" key Frontend.Ast.pp_ty ty));
       Ok env')
;;

let compiler opts : (unit, unit) Result.t =
  let run text env oc =
    if opts.infer_only
    then run_infer_only text env oc
    else run_compile text env oc ~backend:opts.backend ~enable_gc:opts.enable_gc
  in
  let env0 =
    if opts.enable_gc
    then Middleend.Inferencer.TypeEnv.env_with_gc
    else Middleend.Inferencer.TypeEnv.initial_env
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

let parse_args () : (opts, unit) Result.t =
  let parse_backend = function
    | "llvm" -> Ok Llvm
    | "ricsv" -> Ok Ricsv
    | _ -> Error ()
  in
  let rec loop current_opts = function
    | [] -> Ok current_opts
    | "-gc" :: rest -> loop { current_opts with enable_gc = true } rest
    | "-infer" :: rest -> loop { current_opts with infer_only = true } rest
    | "-fromfile" :: path :: rest ->
      loop { current_opts with input_file = Some path } rest
    | "-o" :: path :: rest -> loop { current_opts with output_file = Some path } rest
    | "-backend" :: backend_name :: rest ->
      (match parse_backend backend_name with
       | Ok backend -> loop { current_opts with backend } rest
       | Error () -> Error ())
    | argument :: _ when String.length argument > 0 && Char.equal argument.[0] '-' ->
      Error ()
    | _positional_argument :: _ -> Error ()
  in
  let argv = Array.to_list Sys.argv in
  match argv with
  | [] -> Ok default_opts
  | _program_name :: arguments -> loop default_opts arguments
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
