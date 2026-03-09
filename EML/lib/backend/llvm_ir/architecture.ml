(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm

module Llvm_backend = struct
  type instr =
    | Add of llvalue * llvalue * string
    | Sub of llvalue * llvalue * string
    | Mul of llvalue * llvalue * string
    | Sdiv of llvalue * llvalue * string
    | Neg of llvalue * string
    | Icmp of Icmp.t * llvalue * llvalue * string
    | And of llvalue * llvalue * string
    | Or of llvalue * llvalue * string
    | Not of llvalue * string
    | Load of lltype * llvalue * string
    | Store of llvalue * llvalue
    | Alloca of lltype * string
    | Call of lltype * llvalue * llvalue array * string
    | Ret of llvalue option
    | Br of llbasicblock
    | CondBr of llvalue * llbasicblock * llbasicblock
    | Phi of (llvalue * llbasicblock) list * string
    | Bitcast of llvalue * lltype * string
    | PtrToInt of llvalue * lltype * string
    | IntToPtr of llvalue * lltype * string

  let emit builder = function
    | Add (left, right, name) -> Some (build_add left right name builder)
    | Sub (left, right, name) -> Some (build_sub left right name builder)
    | Mul (left, right, name) -> Some (build_mul left right name builder)
    | Sdiv (left, right, name) -> Some (build_sdiv left right name builder)
    | Neg (operand, name) -> Some (build_neg operand name builder)
    | Icmp (cond, left, right, name) -> Some (build_icmp cond left right name builder)
    | And (left, right, name) -> Some (build_and left right name builder)
    | Or (left, right, name) -> Some (build_or left right name builder)
    | Not (operand, name) -> Some (build_not operand name builder)
    | Load (load_ty, ptr_value, name) -> Some (build_load load_ty ptr_value name builder)
    | Store (value, ptr_value) ->
      let (_ : Llvm.llvalue) = build_store value ptr_value builder in
      None
    | Alloca (alloca_ty, name) -> Some (build_alloca alloca_ty name builder)
    | Call (ft, callee, args, name) -> Some (build_call ft callee args name builder)
    | Ret None ->
      let (_ : Llvm.llvalue) = build_ret_void builder in
      None
    | Ret (Some ret_value) ->
      let (_ : Llvm.llvalue) = build_ret ret_value builder in
      None
    | Br block ->
      let (_ : Llvm.llvalue) = build_br block builder in
      None
    | CondBr (cond, then_bb, else_bb) ->
      let (_ : Llvm.llvalue) = build_cond_br cond then_bb else_bb builder in
      None
    | Phi (incoming, name) -> Some (build_phi incoming name builder)
    | Bitcast (operand, dest_ty, name) ->
      Some (build_bitcast operand dest_ty name builder)
    | PtrToInt (operand, dest_ty, name) ->
      Some (build_ptrtoint operand dest_ty name builder)
    | IntToPtr (operand, dest_ty, name) ->
      Some (build_inttoptr operand dest_ty name builder)
  ;;

  let add builder left right name = emit builder (Add (left, right, name))
  let sub builder left right name = emit builder (Sub (left, right, name))
  let mul builder left right name = emit builder (Mul (left, right, name))
  let sdiv builder left right name = emit builder (Sdiv (left, right, name))
  let neg builder operand name = emit builder (Neg (operand, name))
  let icmp builder cond left right name = emit builder (Icmp (cond, left, right, name))
  let and_ builder left right name = emit builder (And (left, right, name))
  let or_ builder left right name = emit builder (Or (left, right, name))
  let not builder operand name = emit builder (Not (operand, name))
  let load builder load_ty ptr_value name = emit builder (Load (load_ty, ptr_value, name))
  let alloca builder alloca_ty name = emit builder (Alloca (alloca_ty, name))
  let call builder ft callee args name = emit builder (Call (ft, callee, args, name))
  let phi builder incoming name = emit builder (Phi (incoming, name))

  let bitcast builder operand dest_ty name =
    emit builder (Bitcast (operand, dest_ty, name))
  ;;

  let ptrtoint builder operand dest_ty name =
    emit builder (PtrToInt (operand, dest_ty, name))
  ;;

  let inttoptr builder operand dest_ty name =
    emit builder (IntToPtr (operand, dest_ty, name))
  ;;

  let store builder value ptr_value =
    let (_ : Llvm.llvalue option) = emit builder (Store (value, ptr_value)) in
    ()
  ;;

  let ret_void builder =
    let (_ : Llvm.llvalue option) = emit builder (Ret None) in
    ()
  ;;

  let ret builder ret_value =
    let (_ : Llvm.llvalue option) = emit builder (Ret (Some ret_value)) in
    ()
  ;;

  let br builder block =
    let (_ : Llvm.llvalue option) = emit builder (Br block) in
    ()
  ;;

  let cond_br builder cond then_bb else_bb =
    let (_ : Llvm.llvalue option) = emit builder (CondBr (cond, then_bb, else_bb)) in
    ()
  ;;
end
