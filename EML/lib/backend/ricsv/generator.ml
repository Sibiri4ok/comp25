(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Stdlib.Format
open Frontend.Ast
open Middleend.Anf
open Architecture
open Analysis
open Riscv_backend

type env = (ident, location, String.comparator_witness) Map.t

type state =
  { frame_offset : int
  ; fresh_id : int
  ; arity_map : (ident, int, String.comparator_witness) Map.t
  ; env : env
  ; instr_buffer : instr_item list
  }

type 'a t = state -> ('a * state, string) Result.t

let return x st = Ok (x, st)
let fail e = fun _ -> Error e

let bind m f =
  fun state ->
  match m state with
  | Ok (x, st') -> f x st'
  | Error e -> Error e
;;

let ( let* ) = bind
let get st = Ok (st, st)
let put st = fun _ -> Ok ((), st)

let modify f =
  let* st = get in
  put (f st)
;;

let modify_env f = modify (fun st -> { st with env = f st.env })

let get_env =
  let* st = get in
  return st.env
;;

let set_env env = modify (fun st -> { st with env })

let fresh =
  let modify_fresh_id f = modify (fun st -> { st with fresh_id = f st.fresh_id }) in
  let* st = get in
  let* () = modify_fresh_id Int.succ in
  return st.fresh_id
;;

let run m init = m init

let append (items : instr_item list) =
  let modify_instr_buffer f =
    modify (fun st -> { st with instr_buffer = f st.instr_buffer })
  in
  if List.is_empty items
  then return ()
  else
    modify_instr_buffer (fun l ->
      List.fold_left items ~init:l ~f:(fun acc it -> it :: acc))
;;

let alloc_frame_slot =
  let modify_frame_offset f =
    modify (fun st -> { st with frame_offset = f st.frame_offset })
  in
  let* () = modify_frame_offset (fun n -> n + word_size) in
  let* st = get in
  return (fp, -st.frame_offset)
;;

let gen_store reg =
  let* slot = alloc_frame_slot in
  let* () = append (sd reg slot) in
  return (Loc_mem slot)
;;

let gen_load_reg_into dst_reg loc =
  let* () = append (load_reg dst_reg loc) in
  return ()
;;

let caller_saved_locs env =
  Map.to_alist env
  |> List.filter_map ~f:(fun (name, loc) ->
    match loc with
    | Loc_reg r when is_caller_saved r -> Some (name, r)
    | _ -> None)
;;

let gen_save_caller_regs =
  let* env = get_env in
  let regs = caller_saved_locs env in
  let frame_size = List.length regs * word_size in
  let* () =
    if frame_size > 0
    then append (addi sp sp (-frame_size))
    else return ()
  in
  let rec save_regs env = function
    | [] -> return env
    | (name, r) :: rest ->
      let* new_loc = gen_store r in
      save_regs (Map.set env ~key:name ~data:new_loc) rest
  in
  let* new_env = save_regs env regs in
  set_env new_env
;;

let ensure_reg_free dst =
  let reg_is_used env r =
    Map.exists env ~f:(function
      | Loc_reg r' -> equal_reg r r'
      | Loc_mem _ -> false)
  in
  let relocate env ~from ~to_ =
    Map.map env ~f:(function
      | Loc_reg r when equal_reg r from -> to_
      | loc -> loc)
  in
  let* env = get_env in
  if not (reg_is_used env dst)
  then return ()
  else (
    match List.find candidate_regs_for_spill ~f:(fun r -> not (reg_is_used env r)) with
    | Some new_reg ->
      let* () = append (mv new_reg dst) in
      let new_env = relocate env ~from:dst ~to_:(Loc_reg new_reg) in
      set_env new_env
    | None ->
      let* new_loc = gen_store dst in
      let new_env = relocate env ~from:dst ~to_:new_loc in
      set_env new_env)
;;

let bin_oper_to_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | Division -> "/"
  | And -> "&&"
  | Or -> "||"
  | Equal -> "="
  | NotEqual -> "<>"
  | GreaterThan -> ">"
  | LowerThan -> "<"
  | GretestEqual -> ">="
  | LowestEqual -> "<="
;;

let is_rewrites_regs_cache state = function
  | ImmediateConst _ -> false
  | ImmediateVar id -> Map.mem state.arity_map id
;;

let load_imm dst = function
  | ImmediateConst (ConstInt n) -> append (li dst (tag_int n))
  | ImmediateConst (ConstBool b) -> append (li dst (if b then tag_int 1 else tag_int 0))
  | ImmediateConst (ConstChar c) -> append (li dst (tag_int (Char.to_int c)))
  | ImmediateConst (ConstString _) -> fail "String constants not yet supported in codegen"
  | ImmediateVar name ->
    let* env = get_env in
    (match Map.find env name with
     | Some loc -> gen_load_reg_into dst loc
     | None ->
       let* state = get in
       (match Map.find state.arity_map name with
        | Some 0 -> append (call name)
        | Some arity ->
          let* () = append (la result_reg name) in
          let* () = append (li (List.nth_exn arg_regs 1) arity) in
          let* () = append (call "alloc_closure") in
          if equal_reg dst result_reg then return () else append (mv dst result_reg)
        | None -> fail ("unbound variable: " ^ name)))
;;

let copy_result_to dst =
  if equal_reg dst result_reg then return () else append (mv dst result_reg)
;;

let dangerous_arg_indices state exps =
  List.filter_mapi exps ~f:(fun i arg ->
    if is_rewrites_regs_cache state arg then Some i else None)
;;

let spill_dangerous_args ~gen_immediate state exps =
  let dangerous_idxs = dangerous_arg_indices state exps in
  let spill_slots = List.length dangerous_idxs * word_size in
  let* () =
    if spill_slots > 0 then append (addi sp sp (-spill_slots)) else return ()
  in
  List.foldi exps ~init:(return Map.Poly.empty) ~f:(fun i acc arg ->
    let* spilled = acc in
    if List.mem dangerous_idxs i ~equal:Int.equal
    then
      let* () = gen_immediate result_reg arg in
      let* loc = gen_store result_reg in
      return (Map.set spilled ~key:i ~data:loc)
    else return spilled)
;;

let load_exps_into_regs ~gen_immediate spilled_locs arg_regs exps =
  let n = min (List.length exps) (List.length arg_regs) in
  List.foldi (List.take exps n) ~init:(return ()) ~f:(fun i acc arg ->
    let* () = acc in
    let reg = List.nth_exn arg_regs i in
    match Map.find spilled_locs i with
    | Some loc -> gen_load_reg_into reg loc
    | None -> gen_immediate reg arg)
;;

let push_stack_args ~gen_immediate stack_args =
  let n = List.length stack_args in
  if n = 0
  then return 0
  else (
    let storage_for_stack_args = n * word_size in
    let* () = append (addi sp sp (-storage_for_stack_args)) in
    let* () =
      List.foldi stack_args ~init:(return ()) ~f:(fun i acc arg ->
        let* () = acc in
        let offset = i * word_size in
        let* () = gen_immediate t0 arg in
        append (sd t0 (sp, offset)))
    in
    return storage_for_stack_args)
;;

type call_style =
  | Nullary of string
  | Direct of { fname : string; args : immediate list }
  | Via_closure of { fname : string; nargs : int; args : immediate list }

(** Classify how to call fname with the given args.
    - Nullary: zero-arg function called with unit sentinel
    - Direct: known function with exact arity match
    - Via_closure: partial/over application or unknown — use alloc_closure + eml_applyN *)
let classify_call ~nargs ~callee_arity_opt ~fname ~args =
  match callee_arity_opt with
  | Some 0 when nargs = 1 -> return (Nullary fname)
  | Some arity when nargs = arity -> return (Direct { fname; args })
  | _ -> return (Via_closure { fname; nargs; args })
;;

let gen_call_with_regs dst regs args spilled symbol =
  let* () = load_exps_into_regs ~gen_immediate:load_imm spilled regs args in
  let stack_args = List.drop args (List.length regs) in
  let* reserved = push_stack_args ~gen_immediate:load_imm stack_args in
  let* () = append (call symbol) in
  let* () = copy_result_to dst in
  if reserved > 0 then append (addi sp sp reserved) else return ()
;;

let gen_nullary dst fname =
  let* () = append (call fname) in
  copy_result_to dst
;;

let gen_direct_call dst fname args spilled =
  gen_call_with_regs dst arg_regs args spilled fname
;;

(** Push all args onto the stack and call eml_applyN(closure_in_a0, argc, sp).
    Signature: eml_applyN(closure*, int64_t argc, void** argv)
    Precondition: closure pointer is in result_reg (a0) on entry. *)
let gen_apply_via_stack dst argc args =
  (* Save closure pointer: push it on stack temporarily *)
  let* () = append (addi sp sp (-word_size)) in
  let* () = append (sd result_reg (sp, 0)) in
  (* Allocate stack space for argv array *)
  let stack_bytes = argc * word_size in
  let* () = append (addi sp sp (-stack_bytes)) in
  (* Store each arg into the argv array *)
  let* () =
    List.foldi args ~init:(return ()) ~f:(fun i acc arg ->
      let* () = acc in
      let offset = i * word_size in
      let* () = load_imm t0 arg in
      append (sd t0 (sp, offset)))
  in
  (* Restore closure pointer into a0 *)
  let* () = append (ld result_reg (sp, stack_bytes)) in
  (* a1 = argc, a2 = argv pointer (= sp) *)
  let* () = append (li (List.nth_exn arg_regs 1) argc) in
  let* () = append (mv (List.nth_exn arg_regs 2) sp) in
  let* () = append (call "eml_applyN") in
  let* () = copy_result_to dst in
  (* Free argv array + the saved closure slot *)
  append (addi sp sp (stack_bytes + word_size))
;;

(** Call fname via eml_applyN: first alloc a closure then apply args to it.
    [fname_arity] is the real declared arity of fname (passed to alloc_closure).
    [nargs] is how many args we are applying right now. *)
let gen_via_apply dst fname fname_arity nargs args =
  let* () = append (la result_reg fname) in
  let* () = append (li (List.nth_exn arg_regs 1) fname_arity) in
  let* () = append (call "alloc_closure") in
  gen_apply_via_stack dst nargs args
;;

let rec gen_invocation dst fname args =
  let* () = gen_save_caller_regs in
  let* state = get in
  let* env = get_env in
  let nargs = List.length args in
  let callee_arity_opt = Map.find state.arity_map fname in
  let* style = classify_call ~nargs ~callee_arity_opt ~fname ~args in
  match style with
  | Nullary name -> gen_nullary dst name
  | Direct { fname = fn; args = a } ->
    let* spilled = spill_dangerous_args ~gen_immediate:load_imm state a in
    gen_direct_call dst fn a spilled
  | Via_closure { fname = fn; nargs = n; args = a } ->
    (match Map.find env fn with
     | Some loc ->
       (* fn is already a closure value in env: load it into a0, then apply *)
       let* () = gen_load_reg_into result_reg loc in
       gen_apply_via_stack dst n a
     | None ->
       (* fn is a top-level function: allocate fresh closure with its real arity, then apply *)
       let fname_arity = Option.value (Map.find state.arity_map fn) ~default:n in
       gen_via_apply dst fn fname_arity n a)

and gen_unit dst = append (li dst (tag_int 0))
and gen_imm dst imm = load_imm dst imm

and gen_neg dst op =
  let* () = load_imm t0 op in
  let* () = append (li dst (tag_int 0)) in
  append (sub dst dst t0)

and gen_not dst op =
  let* () = load_imm t0 op in
  append (xori dst t0 (tag_int 1))

and gen_binop dst op left right =
  let* () = load_imm t0 left in
  let* () = load_imm t1 right in
  let* () = ensure_reg_free dst in
  append (bin_op dst (bin_oper_to_string op) t0 t1)

and gen_branch dst cond then_e else_e =
  let fresh_branch_labels =
    let* id = fresh in
    return (Printf.sprintf "else_%d" id, Printf.sprintf "end_%d" id)
  in
  let* () = load_imm t0 cond in
  let* else_lbl, end_lbl = fresh_branch_labels in
  let* () = append (beq t0 zero else_lbl) in
  let* st = get in
  let frame_before_then = st.frame_offset in
  let* () = gen_anf dst then_e in
  let* () = append (j end_lbl) in
  let* st_after_then = get in
  let* () =
    put
      { st with
        frame_offset = frame_before_then
      ; instr_buffer = st_after_then.instr_buffer
      }
  in
  let* () = append (label else_lbl) in
  let* () = gen_anf dst else_e in
  append (label end_lbl)

and gen_app dst fname first rest = gen_invocation dst fname (first :: rest)

and gen_cexpr dst = function
  | ComplexUnit -> gen_unit dst
  | ComplexImmediate imm -> gen_imm dst imm
  | ComplexUnarOper (Negative, op) -> gen_neg dst op
  | ComplexUnarOper (Not, op) -> gen_not dst op
  | ComplexBinOper (op, left, right) -> gen_binop dst op left right
  | ComplexBranch (cond, then_e, else_e) -> gen_branch dst cond then_e else_e
  | ComplexField _ | ComplexTuple _ -> fail "Tuple not supported"
  | ComplexApp (ImmediateVar name, first, rest) -> gen_app dst name first rest
  | ComplexApp (closure_imm, first, rest) ->
    (* Calling a closure value: load it into a0, put args on stack, call eml_applyN *)
    let args = first :: rest in
    let nargs = List.length args in
    let* () = gen_save_caller_regs in
    let* () = load_imm result_reg closure_imm in
    gen_apply_via_stack dst nargs args
  | ComplexLambda _ | ComplexList _ | ComplexOption _ ->
    fail "gen_cexpr: Lambda/List/Option not implemented"

and gen_anf dst = function
  | AnfExpr cexp -> gen_cexpr dst cexp
  | AnfLet (_, name, rhs, cont) ->
    let* () = gen_cexpr result_reg rhs in
    let* loc = gen_store result_reg in
    let* () = modify_env (Map.set ~key:name ~data:loc) in
    gen_anf dst cont
;;

let bind_param_reg env i = function
  | ImmediateVar name ->
    let r = List.nth_exn arg_regs i in
    return (Map.set env ~key:name ~data:(Loc_reg r))
  | _ -> fail "unsupported pattern"
;;

let bind_param_stack env i = function
  | ImmediateVar name ->
    let off = (i + 2) * word_size in
    return (Map.set env ~key:name ~data:(Loc_mem (fp, off)))
  | _ -> fail "unsupported pattern"
;;

let flush_queue ppf =
  let get_instr_buffer =
    let* st = get in
    return st.instr_buffer
  in
  let clear_instr_buffer = modify (fun st -> { st with instr_buffer = [] }) in
  let* buf = get_instr_buffer in
  let* () = clear_instr_buffer in
  let () = List.iter (List.rev buf) ~f:(fun (i, _) -> format_item ppf i) in
  return ()
;;

let gen_func f_id params body frame_sz ppf =
  fprintf ppf "\n  .globl %s\n  .type %s, @function\n" f_id f_id;
  let arity = List.length params in
  let params_reg, params_stack = List.split_n params (min arity arg_regs_count) in
  let env0 = Map.empty (module String) in
  let* env =
    List.foldi params_reg ~init:(return env0) ~f:(fun i acc p ->
      let* e = acc in
      bind_param_reg e i p)
  in
  let* env =
    List.foldi params_stack ~init:(return env) ~f:(fun i acc p ->
      let* e = acc in
      bind_param_stack e i p)
  in
  let* () = set_env env in
  let* () = append (prologue ~name:f_id ~stack_size:frame_sz) in
  let* st = get in
  let* () = put { st with frame_offset = 0 } in
  let* () = gen_anf result_reg body in
  let* () = append (epilogue ~is_main:(String.equal f_id "main")) in
  let* () = flush_queue ppf in
  return ()
;;

let gen_program ppf (analysis : analysis_result) =
  fprintf ppf ".section .text";
  let base = Config.base_arities in
  let arity_map =
    List.fold base ~init:analysis.arity_map ~f:(fun map prim ->
      Map.set map ~key:prim.name ~data:prim.arity)
  in
  let init =
    { frame_offset = 0
    ; fresh_id = 0
    ; arity_map
    ; env = Map.empty (module String)
    ; instr_buffer = []
    }
  in
  let comp =
    List.fold analysis.functions ~init:(return ()) ~f:(fun acc fn ->
      let frame_sz = (2 + fn.slots_count) * word_size in
      let* () = acc in
      gen_func fn.f_id fn.params fn.body frame_sz ppf)
  in
  match run comp init with
  | Ok ((), _) -> pp_print_flush ppf ()
  | Error msg ->
    Stdlib.Format.eprintf "Codegen error: %s\n%!" msg;
    Stdlib.exit 1
;;
