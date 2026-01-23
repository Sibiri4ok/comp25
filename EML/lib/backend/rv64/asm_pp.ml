(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Riscv
open Format

let pp_reg fmt = function
  | Zero -> fprintf fmt "zero"
  | Ra -> fprintf fmt "ra"
  | Sp -> fprintf fmt "sp"
  | Fp -> fprintf fmt "fp"
  | A i -> fprintf fmt "a%d" i
  | T i -> fprintf fmt "t%d" i
  | S i -> fprintf fmt "s%d" i
;;

let pp_instr fmt = function
  | Label l -> fprintf fmt "%s:" l
  | Comment s -> fprintf fmt " # %s" s
  | Li (r, n) -> fprintf fmt " li %a, %d" pp_reg r n
  | La (r, l) -> fprintf fmt " la %a, %s" pp_reg r l
  | Mv (d, s) -> fprintf fmt " mv %a, %a" pp_reg d pp_reg s
  | Addi (d, s, n) -> fprintf fmt " addi %a, %a, %d" pp_reg d pp_reg s n
  | Andi (d, s, n) -> fprintf fmt " andi %a, %a, %d" pp_reg d pp_reg s n
  | Add (d, a, b) -> fprintf fmt " add %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Sub (d, a, b) -> fprintf fmt " sub %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Mul (d, a, b) -> fprintf fmt " mul %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Srai (d, s, n) -> fprintf fmt " srai %a, %a, %d" pp_reg d pp_reg s n
  | Srli (d, s, n) -> fprintf fmt " srli %a, %a, %d" pp_reg d pp_reg s n
  | Slli (d, s, n) -> fprintf fmt " slli %a, %a, %d" pp_reg d pp_reg s n
  | Xori (d, a, n) -> fprintf fmt " xori %a, %a, %d" pp_reg d pp_reg a n
  | Ori (d, a, n) -> fprintf fmt " ori %a, %a, %d" pp_reg d pp_reg a n
  | Slt (d, a, b) -> fprintf fmt " slt %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Sd (s, o, b) -> fprintf fmt " sd %a, %d(%a)" pp_reg s o pp_reg b
  | Ld (d, o, b) -> fprintf fmt " ld %a, %d(%a)" pp_reg d o pp_reg b
  | Beqz (r, l) -> fprintf fmt " beqz %a, %s" pp_reg r l
  | Blt (r1, r2, l) -> fprintf fmt " blt %a, %a, %s" pp_reg r1 pp_reg r2 l
  | J l -> fprintf fmt " j %s" l
  | Jalr (rd, rs1, imm) -> fprintf fmt " jalr %a, %d(%a)" pp_reg rd imm pp_reg rs1
  | Call l -> fprintf fmt " call %s" l
  | Ret -> fprintf fmt " ret"
  | Ecall -> fprintf fmt " ecall"
  | Align n -> fprintf fmt " .align %d" n
;;

let pp_program fmt { text; data; global_funs } =
  fprintf fmt ".section .text@\n";
  let rec uniq = function
    | [] -> []
    | x :: xs -> x :: uniq (List.filter (( <> ) x) xs)
  in
  let globals = uniq ("_start" :: "malloc" :: global_funs) in
  List.iter (fun name -> fprintf fmt ".globl %s@\n" name) globals;
  fprintf fmt "@\n";
  List.iter (fun i -> fprintf fmt "%a@\n" pp_instr i) text;
  if data <> []
  then (
    fprintf fmt "@\n.section .data@\n";
    fprintf fmt ".balign 8@\n";
    List.iter
      (function
        | DWord name -> fprintf fmt ".globl %s@\n%s: .dword 0@\n" name name)
      data)
;;
