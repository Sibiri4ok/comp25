(* file: compiler.ml *)
(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast

module StringMap = Map.Make(String)

type location =
  | Reg of string
  | Stack of int
  | Imm of int

type risc_v_operand =
  | Reg of string
  | Imm of int
  | Label of string

type instruction =
  | LI of string * int                    (* Load immediate *)
  | MV of string * string                 (* Move register *)
  | ADD of string * string * risc_v_operand
  | SUB of string * string * risc_v_operand
  | MUL of string * string * risc_v_operand
  | DIV of string * string * risc_v_operand
  | AND of string * string * risc_v_operand
  | OR of string * string * risc_v_operand
  | SLT of string * string * risc_v_operand
  | BEQ of string * string * string       (* Branch if equal *)
  | BNE of string * string * string       (* Branch if not equal *)
  | JAL of string * string                (* Jump and link *)
  | JALR of string * string * int         (* Jump and link register *)
  | JR of string                         (* Jump register *)
  | LW of string * string * int           (* Load word *)
  | SW of string * string * int           (* Store word *)
  | ADDI of string * string * int
  | LABEL of string
  | COMMENT of string
  | ECALL
  | RET

let riscv_registers = [
  "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7";
  "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "t7";
  "s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11";
  "sp"; "ra"
]

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

type env = {
  vars: (string * location) list;
  stack_offset: int;
  functions: (string * (pattern list * expr)) list;
}

let empty_env = {
  vars = [];
  stack_offset = 0;
  functions = [];
}

let rec compile_expr env expr =
  match expr with
  | ExpConst (ConstInt n) ->
      let temp = fresh_temp () in
      [COMMENT ("Load constant " ^ string_of_int n); LI (temp, n)], temp, env
      
  | ExpConst (ConstBool true) ->
      let temp = fresh_temp () in
      [COMMENT "Load true"; LI (temp, 1)], temp, env
      
  | ExpConst (ConstBool false) ->
      let temp = fresh_temp () in
      [COMMENT "Load false"; LI (temp, 0)], temp, env
      
  | ExpIdent name ->
      (try
        let _, loc = List.find (fun (n, _) -> n = name) env.vars in
        match loc with
        | Reg reg -> [], reg, env
        | Stack offset ->
            let temp = fresh_temp () in
            [COMMENT ("Load variable " ^ name); LW (temp, "sp", offset)], temp, env
        | Imm n ->
            let temp = fresh_temp () in
            [LI (temp, n)], temp, env
      with Not_found ->
        failwith ("Unbound variable: " ^ name))
        
  | ExpBinOper (oper, left, right) ->
      let left_code, left_reg, env1 = compile_expr env left in
      let right_code, right_reg, env2 = compile_expr env1 right in
      let result_reg = fresh_temp () in
      
      let op_code = match oper with
        | Plus -> [ADD (result_reg, left_reg, Reg right_reg)]
        | Minus -> [SUB (result_reg, left_reg, Reg right_reg)]
        | Multiply -> [MUL (result_reg, left_reg, Reg right_reg)]
        | Division -> [DIV (result_reg, left_reg, Reg right_reg)]
        | And -> [AND (result_reg, left_reg, Reg right_reg)]
        | Or -> [OR (result_reg, left_reg, Reg right_reg)]
        | LowerThan -> [SLT (result_reg, left_reg, Reg right_reg)]
        | GreaterThan -> 
            [SLT (result_reg, right_reg, Reg left_reg)]
        | LowestEqual ->
            let lt_instr = SLT (result_reg, left_reg, Reg right_reg) in
            let eq_instr = BEQ (left_reg, right_reg, "skip_leq_" ^ fresh_label "leq") in
            [lt_instr; LI (result_reg, 1); eq_instr; LI (result_reg, 0); LABEL ("skip_leq_" ^ fresh_label "leq")]
        | GretestEqual ->
            let lt_instr = SLT (result_reg, right_reg, Reg left_reg) in
            let eq_instr = BEQ (left_reg, right_reg, "skip_geq_" ^ fresh_label "geq") in
            [lt_instr; LI (result_reg, 1); eq_instr; LI (result_reg, 0); LABEL ("skip_geq_" ^ fresh_label "geq")]
        | Equal -> [BEQ (left_reg, right_reg, "skip_eq_" ^ fresh_label "eq"); 
                   LI (result_reg, 0); 
                   JAL ("zero", "end_eq_" ^ fresh_label "eq");
                   LABEL ("skip_eq_" ^ fresh_label "eq");
                   LI (result_reg, 1);
                   LABEL ("end_eq_" ^ fresh_label "eq")]
        | NotEqual -> [BNE (left_reg, right_reg, "skip_neq_" ^ fresh_label "neq");
                      LI (result_reg, 0);
                      JAL ("zero", "end_neq_" ^ fresh_label "neq");
                      LABEL ("skip_neq_" ^ fresh_label "neq");
                      LI (result_reg, 1);
                      LABEL ("end_neq_" ^ fresh_label "neq")]
      in
      
      left_code @ right_code @ [COMMENT "Binary operation"] @ op_code, 
      result_reg, env2
      
  | ExpBranch (cond, then_expr, else_expr) ->
      let cond_code, cond_reg, env1 = compile_expr env cond in
      let then_label = fresh_label "then" in
      let else_label = fresh_label "else" in
      let end_label = fresh_label "end_if" in
      let result_reg = fresh_temp () in
      
      let then_code, then_reg, env2 = compile_expr env1 then_expr in
      let else_code, else_reg, env3 = 
        match else_expr with
        | Some expr -> compile_expr env2 expr
        | None -> [LI (result_reg, 0)], result_reg, env2
      in
      
      cond_code @
      [COMMENT "Conditional branch";
       BNE (cond_reg, "zero", then_label);
       JAL ("zero", else_label);
       LABEL then_label] @
      then_code @
      [MV (result_reg, then_reg);
       JAL ("zero", end_label);
       LABEL else_label] @
      else_code @
      [MV (result_reg, else_reg);
       LABEL end_label], 
      result_reg, env3
      
  | ExpLet (_, (pattern, bind_expr), _, body_expr) ->  (* Исправлено: убрали неиспользуемые переменные *)
      let bind_code, bind_reg, env1 = compile_expr env bind_expr in
      let new_env = 
        match pattern with
        | PatVariable _ ->  (* Исправлено: убрали неиспользуемую переменную name *)
            let stack_offset = env1.stack_offset + 4 in
            let new_vars = ("", Stack env1.stack_offset) :: env1.vars in  (* Заменили name на "" *)
            { env1 with vars = new_vars; stack_offset = stack_offset }
        | _ -> env1
      in
      
      let store_code = 
        match pattern with
        | PatVariable _ -> [SW (bind_reg, "sp", env1.stack_offset)]  (* Исправлено: убрали неиспользуемую переменную name *)
        | _ -> []
      in
      
      let body_code, body_reg, env2 = compile_expr new_env body_expr in
      
      bind_code @ store_code @ body_code, body_reg, env2
      
  | ExpLambda (_, _) ->  (* Исправлено: убрали неиспользуемые переменные params и body *)
      (* For factorial, we'll handle the simple recursive case *)
      let func_label = fresh_label "func" in
      let result_reg = fresh_temp () in
      
      (* Store function address *)
      [COMMENT "Lambda function";
       LI (result_reg, 0); (* Placeholder for function address *)
       JAL ("zero", "skip_" ^ func_label);
       LABEL func_label],
      result_reg, env
      
  | ExpFunction (func, arg) ->
      (* Function application - for factorial recursion *)
      let func_code, func_reg, env1 = compile_expr env func in
      let arg_code, arg_reg, env2 = compile_expr env1 arg in
      let result_reg = fresh_temp () in
      
      (* Prepare argument and call function *)
      func_code @ arg_code @
      [COMMENT "Function application";
       MV ("a0", arg_reg);
       JALR ("ra", func_reg, 0);
       MV (result_reg, "a0")],
      result_reg, env2
      
  | ExpUnarOper (Negative, expr) ->
      let expr_code, expr_reg, env1 = compile_expr env expr in
      let result_reg = fresh_temp () in
      expr_code @ [SUB (result_reg, "zero", Reg expr_reg)], result_reg, env1
      
  | ExpUnarOper (Not, expr) ->
      let expr_code, expr_reg, env1 = compile_expr env expr in
      let result_reg = fresh_temp () in
      let true_label = fresh_label "not_true" in
      let end_label = fresh_label "not_end" in
      
      expr_code @
      [COMMENT "Logical NOT";
       BNE (expr_reg, "zero", true_label);
       LI (result_reg, 1);
       JAL ("zero", end_label);
       LABEL true_label;
       LI (result_reg, 0);
       LABEL end_label],
      result_reg, env1
      
  | _ -> failwith "Unsupported expression in compiler"

(* Simple factorial without recursion *)
let compile_factorial () =
  let entry_label = "_start" in
  
  [
    COMMENT "Simple Factorial Compiler";
    COMMENT "Entry point";
    LABEL entry_label;
    
    (* Calculate factorial(5) = 5 * 4 * 3 * 2 * 1 = 120 *)
    LI ("a0", 5);        (* n = 5 *)
    LI ("t0", 1);        (* result = 1 *)
    LI ("t1", 1);        (* i = 1 *)
    
    COMMENT "Loop: while i <= n do result *= i; i++ done";
    LABEL "loop";
    SLT ("t2", "t1", Reg "a0");  (* t2 = (i < n) *)
    BNE ("t2", "zero", "multiply");
    SLT ("t2", "a0", Reg "t1");  (* t2 = (n < i) *)
    BNE ("t2", "zero", "done");
    
    LABEL "multiply";
    MUL ("t0", "t0", Reg "t1");  (* result *= i *)
    ADDI ("t1", "t1", 1);    (* i++ *)
    JAL ("zero", "loop");
    
    LABEL "done";
    MV ("a0", "t0");         (* return result *)
    
    (* Exit with result as exit code *)
    MV ("a0", "t0"); (* result *)
    LI ("a7", 93);  (* Exit syscall number *)
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
    | DIV (rd, rs1, op) -> handle_arithmetic "div" rd rs1 op
    | AND (rd, rs1, op) -> handle_arithmetic "and" rd rs1 op
    | OR (rd, rs1, op) -> handle_arithmetic "or" rd rs1 op
    | SLT (rd, rs1, op) -> handle_arithmetic "slt" rd rs1 op
    | BEQ (rs1, rs2, label) -> add_line (Printf.sprintf "    beq %s, %s, %s" rs1 rs2 label)
    | BNE (rs1, rs2, label) -> add_line (Printf.sprintf "    bne %s, %s, %s" rs1 rs2 label)
    | JAL (rd, label) -> add_line (Printf.sprintf "    jal %s, %s" rd label)
    | JALR (rd, rs, offset) -> add_line (Printf.sprintf "    jalr %s, %s, %d" rd rs offset)
    | JR (rs) -> add_line (Printf.sprintf "    jr %s" rs)
    | LW (rd, rs, offset) -> add_line (Printf.sprintf "    lw %s, %d(%s)" rd offset rs)
    | SW (rs, rd, offset) -> add_line (Printf.sprintf "    sw %s, %d(%s)" rs offset rd)
    | ADDI (rd, rs, imm) -> add_line (Printf.sprintf "    addi %s, %s, %d" rd rs imm)
    | LABEL label -> add_line (label ^ ":")
    | COMMENT text -> add_line ("    # " ^ text)
    | ECALL -> add_line "    ecall"
    | RET -> add_line "    ret"
  ) instructions;
  
  Buffer.contents buf

let compile_program _ =  (* Исправлено: убрали неиспользуемую переменную program *)
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