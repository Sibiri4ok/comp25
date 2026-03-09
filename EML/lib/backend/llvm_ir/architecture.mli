(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm

module Llvm_backend : sig
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

  val emit : llbuilder -> instr -> llvalue option
  val add : llbuilder -> llvalue -> llvalue -> string -> llvalue option
  val sub : llbuilder -> llvalue -> llvalue -> string -> llvalue option
  val mul : llbuilder -> llvalue -> llvalue -> string -> llvalue option
  val sdiv : llbuilder -> llvalue -> llvalue -> string -> llvalue option
  val neg : llbuilder -> llvalue -> string -> llvalue option
  val icmp : llbuilder -> Icmp.t -> llvalue -> llvalue -> string -> llvalue option
  val and_ : llbuilder -> llvalue -> llvalue -> string -> llvalue option
  val or_ : llbuilder -> llvalue -> llvalue -> string -> llvalue option
  val not : llbuilder -> llvalue -> string -> llvalue option
  val load : llbuilder -> lltype -> llvalue -> string -> llvalue option
  val alloca : llbuilder -> lltype -> string -> llvalue option
  val call : llbuilder -> lltype -> llvalue -> llvalue array -> string -> llvalue option
  val phi : llbuilder -> (llvalue * llbasicblock) list -> string -> llvalue option
  val bitcast : llbuilder -> llvalue -> lltype -> string -> llvalue option
  val ptrtoint : llbuilder -> llvalue -> lltype -> string -> llvalue option
  val inttoptr : llbuilder -> llvalue -> lltype -> string -> llvalue option
  val store : llbuilder -> llvalue -> llvalue -> unit
  val ret_void : llbuilder -> unit
  val ret : llbuilder -> llvalue -> unit
  val br : llbuilder -> llbasicblock -> unit
  val cond_br : llbuilder -> llvalue -> llbasicblock -> llbasicblock -> unit
end
