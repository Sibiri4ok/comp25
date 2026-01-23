(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type reg =
  | Zero
  | Ra
  | Sp
  | Fp
  | A of int
  | T of int
  | S of int

type instr =
  | Li of reg * int
  | La of reg * string
  | Mv of reg * reg
  | Addi of reg * reg * int
  | Add of reg * reg * reg
  | Andi of reg * reg * int
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Srai of reg * reg * int
  | Srli of reg * reg * int
  | Slli of reg * reg * int
  | Xori of reg * reg * int
  | Ori of reg * reg * int
  | Slt of reg * reg * reg
  | Sd of reg * int * reg
  | Ld of reg * int * reg
  | Beqz of reg * string
  | Blt of reg * reg * string
  | J of string
  | Jalr of reg * reg * int
  | Call of string
  | Ret
  | Label of string
  | Comment of string
  | Ecall
  | Align of int

type data = DWord of string

type program =
  { text : instr list
  ; data : data list
  ; global_funs : string list
  }

let env_reg = S 1
let result_reg = T 0
let temp_reg = T 4 (* Основной временный регистр *)
let temp_reg2 = T 5 (* Дополнительный временный регистр *)
let temp_reg3 = T 6
let closure_ptr_reg = T 3
let code_ptr_reg = T 1
let env_ptr_reg = T 2
let align16 n = if n mod 16 = 0 then n else ((n / 16) + 1) * 16
