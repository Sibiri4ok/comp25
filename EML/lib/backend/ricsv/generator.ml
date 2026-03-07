(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Frontend.Ast
open Middleend.Anf
open Architecture
open Analysis
open Riscv_backend
open Generator_state
open Auxillary

let alloc_frame_slot =
  let modify_frame_offset f =
    modify (fun st -> { st with frame_offset = f st.frame_offset })
  in
  let* () = modify_frame_offset (fun n -> n + word_size) in
  let* st = get in
  return (fp, -st.frame_offset)
;;

let store_reg_into_frame reg =
  let* slot = alloc_frame_slot in
  let* () = append (sd reg slot) in
  return (Loc_mem slot)
;;

let load_into_reg dst_reg loc =
  let instrs =
    match loc with
    | Loc_reg src_reg when equal_reg src_reg dst_reg -> []
    | Loc_reg src_reg -> mv dst_reg src_reg
    | Loc_mem ofs -> ld dst_reg ofs
  in
  let* () = append instrs in
  return ()
;;

(** Spill function parameters to the frame in param order (index 0 → first slot).
    Ensures env maps each param name to a consistent slot so (self l) loads self, not l. *)
let spill_params_to_frame params_reg =
  Base.List.foldi params_reg ~init:(return ()) ~f:(fun i acc p ->
    let* () = acc in
    match p with
    | ImmediateVar name ->
      let r = List.nth arg_regs i in
      let* slot = store_reg_into_frame r in
      modify_env (fun env -> Base.Map.set env ~key:name ~data:slot)
    | _ -> return ())
;;

let spill_caller_saved_vars_to_frame =
  let* env = get_env in
  let vars = vars_in_caller_saved_regs env in
  let frame_bytes = List.length vars * word_size in
  let* () = if frame_bytes > 0 then append (addi sp sp (-frame_bytes)) else return () in
  let rec spill env = function
    | [] -> return env
    | (name, r) :: rest ->
      let* new_loc = store_reg_into_frame r in
      spill (Base.Map.set env ~key:name ~data:new_loc) rest
  in
  let* new_env = spill env vars in
  set_env new_env
;;

let evacuate_reg dst =
  let is_reg_used env r =
    Base.Map.exists env ~f:(fun loc ->
      match loc with
      | Loc_reg r' -> equal_reg r r'
      | Loc_mem _ -> false)
  in
  let rewrite_loc_in_env env from_reg to_loc =
    Base.Map.map env ~f:(function
      | Loc_reg r when equal_reg r from_reg -> to_loc
      | loc -> loc)
  in
  let* env = get_env in
  if not (is_reg_used env dst)
  then return ()
  else (
    match List.find_opt (fun r -> not (is_reg_used env r)) candidate_regs_for_spill with
    | Some new_reg ->
      let* () = append (mv new_reg dst) in
      let new_env = rewrite_loc_in_env env dst (Loc_reg new_reg) in
      set_env new_env
    | None ->
      let* new_loc = store_reg_into_frame dst in
      let new_env = rewrite_loc_in_env env dst new_loc in
      set_env new_env)
;;

let resolve_call_symbol name =
  let* st = get in
  match st.symbol_resolve st.current_func_index name with
  | Some (asm, _) -> return asm
  | None -> return name
;;

let gen_imm dst = function
  | ImmediateConst (ConstInt n) -> append (li dst (tag_int n))
  | ImmediateConst (ConstBool b) -> append (li dst (if b then tag_int 1 else tag_int 0))
  | ImmediateConst (ConstChar c) -> append (li dst (tag_int (Char.code c)))
  | ImmediateConst (ConstString _) -> fail "String constants not yet supported in codegen"
  | ImmediateVar name ->
    let* env = get_env in
    (match Base.Map.find env name with
     | Some loc -> load_into_reg dst loc
     | None ->
       let* state = get in
       let sym, arity =
         match state.symbol_resolve state.current_func_index name with
         | Some (asm_name, a) -> asm_name, a
         | None ->
           (match Base.Map.find state.arity_map name with
            | Some a -> name, a
            | None -> name, -1)
       in
       if arity < 0
       then fail ("unbound variable: " ^ name)
       else (
         match arity with
         | 0 -> append (call sym)
         | n ->
           let* () = append (la result_reg sym) in
           let* () = append (li (List.nth arg_regs 1) n) in
           append (call "alloc_closure")))
;;

let copy_result_to dst =
  if equal_reg dst result_reg then return () else append (mv dst result_reg)
;;

let spill_dangerous_args state exps =
  let dangerous_idxs = indices_of_args_to_spill state exps in
  let spill_slots = List.length dangerous_idxs * word_size in
  let* () = if spill_slots > 0 then append (addi sp sp (-spill_slots)) else return () in
  Base.List.foldi
    exps
    ~init:(return (Base.Map.empty (module Base.Int)))
    ~f:(fun i acc arg ->
      let* spilled = acc in
      if List.mem i dangerous_idxs
      then
        let* () = gen_imm result_reg arg in
        let* loc = store_reg_into_frame result_reg in
        return (Base.Map.add_exn spilled ~key:i ~data:loc)
      else return spilled)
;;

let load_exps_into_regs spilled_locs arg_regs exps =
  let n = min (List.length exps) (List.length arg_regs) in
  Base.List.foldi (Base.List.take exps n) ~init:(return ()) ~f:(fun i acc arg ->
    let* () = acc in
    let reg = List.nth arg_regs i in
    match Base.Map.find spilled_locs i with
    | Some loc -> load_into_reg reg loc
    | None -> gen_imm reg arg)
;;

let push_stack_args stack_args =
  let n = List.length stack_args in
  if n = 0
  then return 0
  else (
    let stack_bytes = n * word_size in
    let* () = append (addi sp sp (-stack_bytes)) in
    let* () =
      Base.List.foldi stack_args ~init:(return ()) ~f:(fun i acc arg ->
        let* () = acc in
        let offset = i * word_size in
        let* () = gen_imm t0 arg in
        append (sd t0 (sp, offset)))
    in
    return stack_bytes)
;;

let gen_call_with_regs dst regs args spilled symbol =
  let* () = load_exps_into_regs spilled regs args in
  let stack_args = Base.List.drop args (List.length regs) in
  let* reserved = push_stack_args stack_args in
  let* () = append (call symbol) in
  let* () = copy_result_to dst in
  if reserved > 0 then append (addi sp sp reserved) else return ()
;;

let gen_nullary dst fname =
  let* sym = resolve_call_symbol fname in
  let* () = append (call sym) in
  copy_result_to dst
;;

let gen_direct_call dst fname args spilled =
  let* sym = resolve_call_symbol fname in
  gen_call_with_regs dst arg_regs args spilled sym
;;

let gen_via_apply_nargs dst fname nargs args spilled =
  let argv_bytes = nargs * word_size in
  let* () = gen_imm a0 (ImmediateVar fname) in
  let* () = append (li a1 nargs) in
  let* () = append (addi sp sp (-argv_bytes)) in
  let* () =
    Base.List.foldi args ~init:(return ()) ~f:(fun i acc arg ->
      let* () = acc in
      let offset = i * word_size in
      let src =
        match Base.Map.find spilled i with
        | Some loc -> load_into_reg t0 loc
        | None -> gen_imm t0 arg
      in
      let* () = src in
      append (sd t0 (sp, offset)))
  in
  let* () = append (mv a2 sp) in
  let* () = append (call "eml_applyN") in
  let* () = copy_result_to dst in
  append (addi sp sp argv_bytes)
;;

let rec gen_invocation dst fname args =
  let* () = spill_caller_saved_vars_to_frame in
  let* state = get in
  let* spilled = spill_dangerous_args state args in
  let nargs = List.length args in
  let callee_arity_opt = Base.Map.find state.arity_map fname in
  let style = classify_call ~nargs ~callee_arity_opt ~fname ~args in
  match style with
  | Nullary name -> gen_nullary dst name
  | Curry_chain { fname = fn; arity; first_args; rest_args } ->
    gen_curried_call dst fn arity first_args rest_args
  | Direct { fname = fn; args = a } -> gen_direct_call dst fn a spilled
  | Via_apply_nargs { fname = fn; nargs = n; args = a } ->
    gen_via_apply_nargs dst fn n a spilled

and gen_curried_call dst fname _arity first_args rest_args =
  let* part_name = fresh_partial in
  let* () =
    gen_cexpr
      dst
      (ComplexApp (ImmediateVar fname, List.hd first_args, List.tl first_args))
  in
  let* loc = store_reg_into_frame dst in
  let* () = modify_env (fun env -> Base.Map.set env ~key:part_name ~data:loc) in
  (* Apply each rest_arg one at a time (eml_applyN expects one application per call) *)
  let rec apply_rest = function
    | [] -> return ()
    | [ arg ] -> gen_cexpr dst (ComplexApp (ImmediateVar part_name, arg, []))
    | arg :: rest ->
      let* () = gen_cexpr dst (ComplexApp (ImmediateVar part_name, arg, [])) in
      let* loc' = store_reg_into_frame dst in
      let* () = modify_env (fun env -> Base.Map.set env ~key:part_name ~data:loc') in
      apply_rest rest
  in
  apply_rest rest_args

and gen_unit dst = append (li dst (tag_int 0))

and gen_neg dst op =
  let* () = gen_imm t0 op in
  let* () = append (li dst (tag_int 0)) in
  append (sub dst dst t0)

and gen_not dst op =
  let* () = gen_imm t0 op in
  append (xori dst t0 (tag_int 1))

and gen_binop dst op left right =
  let* () = gen_imm t0 left in
  let* () = gen_imm t1 right in
  let* () = evacuate_reg dst in
  match bin_op dst (bin_oper_to_string op) t0 t1 with
  | Ok instrs -> append instrs
  | Error msg -> fail msg

and gen_branch dst cond then_e else_e =
  let* () = gen_imm t0 cond in
  let* else_lbl, end_lbl = fresh_branch in
  (* Branch to else when cond equals tagged false (1); not zero *)
  let* () = append (li t1 (tag_int 0)) in
  let* () = append (beq t0 t1 else_lbl) in
  let* st_before_then = get in
  let frame_before_then = st_before_then.frame_offset in
  let* () = gen_anf dst then_e in
  let* () = append (j end_lbl) in
  let* st_after_then = get in
  let* () =
    put
      { st_before_then with
        frame_offset = frame_before_then
      ; instr_buffer = st_after_then.instr_buffer
      }
  in
  let* () = append (label else_lbl) in
  let* () = gen_anf dst else_e in
  append (label end_lbl)

and spill_tuple_var_if_in_reg = function
  | ImmediateVar name ->
    let* env = get_env in
    (match Base.Map.find env name with
     | Some (Loc_reg _) ->
       let* loc = store_reg_into_frame result_reg in
       modify_env (fun env -> Base.Map.set env ~key:name ~data:loc)
     | _ -> return ())
  | _ -> return ()

and gen_field dst tuple_imm idx =
  let* () = gen_imm result_reg tuple_imm in
  let* () = spill_tuple_var_if_in_reg tuple_imm in
  let* () = append (li (List.nth arg_regs 1) (tag_int idx)) in
  let* () = append (call "field") in
  copy_result_to dst

and gen_list dst = function
  | [] -> append (li dst (tag_int 0))
  | hd :: tl ->
    let* () = gen_list dst tl in
    let* tail_loc = store_reg_into_frame dst in
    let* () = gen_imm t0 hd in
    let* () = spill_caller_saved_vars_to_frame in
    let* () = append (li result_reg 2) in
    let* () = load_into_reg (List.nth arg_regs 1) (Loc_reg t0) in
    let* () = load_into_reg (List.nth arg_regs 2) tail_loc in
    let* () = append (call "create_tuple") in
    copy_result_to dst

and gen_tuple dst e1 e2 rest =
  let elts = e1 :: e2 :: rest in
  let argc = List.length elts in
  let* () = spill_caller_saved_vars_to_frame in
  let* state = get in
  let* spilled = spill_dangerous_args state elts in
  let array_bytes = argc * word_size in
  let* () = append (addi sp sp (-array_bytes)) in
  let* () =
    Base.List.foldi elts ~init:(return ()) ~f:(fun i acc elt ->
      let* () = acc in
      let offset = i * word_size in
      let src =
        match Base.Map.find spilled i with
        | Some loc -> load_into_reg t0 loc
        | None -> gen_imm t0 elt
      in
      let* () = src in
      append (sd t0 (sp, offset)))
  in
  let* () = append (li result_reg argc) in
  let* () = append (addi (List.nth arg_regs 1) sp 0) in
  let* () = append (call "create_tuple") in
  let* () = append (addi sp sp array_bytes) in
    copy_result_to dst

and gen_app dst fname first rest = gen_invocation dst fname (first :: rest)

and gen_cexpr dst = function
  | ComplexUnit -> gen_unit dst
  | ComplexImmediate imm -> gen_imm dst imm
  | ComplexUnarOper (Negative, op) -> gen_neg dst op
  | ComplexUnarOper (Not, op) -> gen_not dst op
  | ComplexBinOper (op, left, right) -> gen_binop dst op left right
  | ComplexBranch (cond, then_e, else_e) -> gen_branch dst cond then_e else_e
  | ComplexField (tuple_imm, idx) -> gen_field dst tuple_imm idx
  | ComplexTuple (e1, e2, rest) -> gen_tuple dst e1 e2 rest
  | ComplexApp (ImmediateVar name, first, rest) -> gen_app dst name first rest
  | ComplexApp (_, _, _) -> fail "ComplexApp: function must be a variable"
  | ComplexLambda _ | ComplexOption _ -> fail "gen_cexpr: Lambda/Option not implemented"
  | ComplexList imm_list -> gen_list dst imm_list

and gen_anf dst = function
  | AnfExpr cexp -> gen_cexpr dst cexp
  | AnfLet (_, name, rhs, cont) ->
    let* () = evacuate_reg result_reg in
    let* () = gen_cexpr result_reg rhs in
    let* loc = store_reg_into_frame result_reg in
    let* () = modify_env (fun env -> Base.Map.set env ~key:name ~data:loc) in
    gen_anf dst cont
;;

let bind_param_to_reg env i = function
  | ImmediateVar name ->
    let r = List.nth arg_regs i in
    return (Base.Map.set env ~key:name ~data:(Loc_reg r))
  | _ -> fail "unsupported pattern"
;;

let bind_param_to_stack env i = function
  | ImmediateVar name ->
    let off = (i + 2) * word_size in
    return (Base.Map.set env ~key:name ~data:(Loc_mem (fp, off)))
  | _ -> fail "unsupported pattern"
;;

let flush_instr_buffer ppf =
  let get_instr_buffer =
    let* st = get in
    return st.instr_buffer
  in
  let clear_instr_buffer = modify (fun st -> { st with instr_buffer = [] }) in
  let* buf = get_instr_buffer in
  let* () = clear_instr_buffer in
  let () = List.iter (fun item -> format_item ppf item) (List.rev buf) in
  return ()
;;

let gen_func ~enable_gc func_name params body frame_sz ppf =
  fprintf ppf "\n  .globl %s\n  .type %s, @function\n" func_name func_name;
  let args = List.length params in
  let params_reg, params_stack =
    ( Base.List.take params (min args arg_regs_count)
    , Base.List.drop params (min args arg_regs_count) )
  in
  let env0 = Base.Map.empty (module Base.String) in
  let* env =
    Base.List.foldi params_reg ~init:(return env0) ~f:(fun i acc p ->
      let* e = acc in
      bind_param_to_reg e i p)
  in
  let* env =
    Base.List.foldi params_stack ~init:(return env) ~f:(fun i acc p ->
      let* e = acc in
      bind_param_to_stack e i p)
  in
  let* () = set_env env in
  let* () = append (prologue ~enable_gc ~name:func_name ~stack_size:frame_sz) in
  let* st = get in
  let* () = put { st with frame_offset = 0 } in
  let* () = spill_params_to_frame params_reg in
  let* () = gen_anf result_reg body in
  let* () = append (epilogue ~enable_gc ~is_main:(String.equal func_name "main")) in
  let* () = flush_instr_buffer ppf in
  return ()
;;

let gen_program ?(enable_gc = false) ppf (analysis : analysis_result) =
  fprintf ppf ".section .text";
  let base = Config.primitive_arities ~enable_gc in
  let arity_map =
    List.fold_left
      (fun map { Config.name; arity } ->
         Base.Map.set map ~key:name ~data:arity)
      analysis.arity_map
      base
  in
  let init =
    { frame_offset = 0
    ; naming_state = Default_naming.init
    ; arity_map
    ; env = Base.Map.empty (module Base.String)
    ; instr_buffer = []
    ; current_func_index = 0
    ; symbol_resolve = analysis.resolve
    }
  in
  let comp =
    Base.List.foldi analysis.functions ~init:(return ()) ~f:(fun i acc fn ->
      let frame_sz = (2 + fn.slots_count) * word_size in
      let* () = acc in
      let* () = modify (fun st -> { st with current_func_index = i }) in
      gen_func ~enable_gc fn.asm_name fn.params fn.body frame_sz ppf)
  in
  match run comp init with
  | Ok ((), _) ->
    pp_print_flush ppf ();
    Ok ()
  | Error msg -> Error msg
;;
