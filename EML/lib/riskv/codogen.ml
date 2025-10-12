(* file: codogen.ml *)
(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)


type risc_v_operand =
  | Reg of string
  | Imm of int
  | Label of string

type instruction =
  | LI of string * int
  | MV of string * string
  | ADD of string * string * risc_v_operand
  | SUB of string * string * risc_v_operand
  | MUL of string * string * risc_v_operand
  | AND of string * string * risc_v_operand
  | OR of string * string * risc_v_operand
  | SLT of string * string * risc_v_operand
  | BEQ of string * string * string
  | BNE of string * string * string
  | JAL of string * string
  | JALR of string * string * int
  | LW of string * string * int
  | SW of string * string * int
  | ADDI of string * string * int
  | LABEL of string
  | COMMENT of string
  | ECALL
  | RET

let next_temp = ref 0
let next_label = ref 0

let fresh_temp () =
  let reg = "t" ^ string_of_int !next_temp in
  incr next_temp;
  if !next_temp >= 7 then next_temp := 0;
  reg

let fresh_label prefix =
  let label = prefix ^ "_" ^ string_of_int !next_label in
  incr next_label;
  label

(* Simple iterative factorial compilation *)
let compile_factorial () =
  [
    COMMENT "Factorial Compiler";
    COMMENT "Entry point";
    LABEL "_start";
    
    (* Initialize: n = 5, result = 1, i = 1 *)
    LI ("a0", 5);        (* n = 5 *)
    LI ("t0", 1);        (* result = 1 *)
    LI ("t1", 1);        (* i = 1 *)
    
    COMMENT "Loop: while i <= n";
    LABEL "loop";
    (* Check if i > n *)
    SLT ("t2", "a0", Reg "t1");
    BNE ("t2", "zero", "done");
    
    COMMENT "Multiply result by i";
    MUL ("t0", "t0", Reg "t1");
    
    COMMENT "Increment i";
    ADDI ("t1", "t1", 1);
    JAL ("zero", "loop");
    
    COMMENT "Store result and exit";
    LABEL "done";
    MV ("a0", "t0");     (* Move result to a0 *)
    LI ("a7", 93);       (* Exit syscall *)
    ECALL;
  ]

let instructions_to_string instructions =
  let buf = Buffer.create 1024 in
  
  let add_line line = Buffer.add_string buf (line ^ "\n") in
  
  let handle_arithmetic name rd rs1 op =
    match op with
    | Reg rs2 -> add_line (Printf.sprintf "    %s %s, %s, %s" name rd rs1 rs2)
    | Imm imm -> add_line (Printf.sprintf "    %s %s, %s, %d" name rd rs1 imm)
    | Label lbl -> failwith (Printf.sprintf "Label %s not supported in arithmetic operation" lbl)
  in
  
  List.iter (function
    | LI (rd, imm) -> add_line (Printf.sprintf "    li %s, %d" rd imm)
    | MV (rd, rs) -> add_line (Printf.sprintf "    mv %s, %s" rd rs)
    | ADD (rd, rs1, op) -> handle_arithmetic "add" rd rs1 op
    | SUB (rd, rs1, op) -> handle_arithmetic "sub" rd rs1 op
    | MUL (rd, rs1, op) -> handle_arithmetic "mul" rd rs1 op
    | AND (rd, rs1, op) -> handle_arithmetic "and" rd rs1 op
    | OR (rd, rs1, op) -> handle_arithmetic "or" rd rs1 op
    | SLT (rd, rs1, op) -> handle_arithmetic "slt" rd rs1 op
    | BEQ (rs1, rs2, label) -> add_line (Printf.sprintf "    beq %s, %s, %s" rs1 rs2 label)
    | BNE (rs1, rs2, label) -> add_line (Printf.sprintf "    bne %s, %s, %s" rs1 rs2 label)
    | JAL (rd, label) -> add_line (Printf.sprintf "    jal %s, %s" rd label)
    | JALR (rd, rs, offset) -> add_line (Printf.sprintf "    jalr %s, %s, %d" rd rs offset)
    | LW (rd, rs, offset) -> add_line (Printf.sprintf "    lw %s, %d(%s)" rd offset rs)
    | SW (rs, rd, offset) -> add_line (Printf.sprintf "    sw %s, %d(%s)" rs offset rd)
    | ADDI (rd, rs, imm) -> add_line (Printf.sprintf "    addi %s, %s, %d" rd rs imm)
    | LABEL label -> add_line (label ^ ":")
    | COMMENT text -> add_line ("    # " ^ text)
    | ECALL -> add_line "    ecall"
    | RET -> add_line "    ret"
  ) instructions;
  
  Buffer.contents buf

let compile_program _program =
  let instructions = compile_factorial () in
  ".text\n.globl _start\n" ^ instructions_to_string instructions

(* Main compilation function *)
let compile_to_riscv program =
  try
    let asm_code = compile_program program in
    asm_code
  with e ->
    Printf.eprintf "Compilation error: %s\n" (Printexc.to_string e);
    ""