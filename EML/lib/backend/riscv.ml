(** Copyright 2025-2026, Gleb Nasretdinov, Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open EML_lib.Frontend.Ast
open Machine  (* This is in the same backend library *)
open EML_lib.Middleend.Anf

type location = Stack of int [@@warning "-37"]

let word_size = 8

module M = struct
  open Base

  type env = (string, location, String.comparator_witness) Map.t

 type st =
    { env : env
    ; frame_offset : int
    ; fresh : int
    }

  type 'a t = st -> st * 'a

  let return (x : 'a) : 'a t = fun s -> s, x

  let ( >>= ) (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun s ->
      let s', a = m s in
      f a s'

  let get : st t = fun s -> s, s

  let put (s : st) : unit t = fun _ -> s, ()

  let modify (f : st -> st) : unit t =
    fun s -> let s' = f s in s', ()

  let ( let* ) = ( >>= )
  let ( let+ ) m f = m >>= fun x -> return (f x)
  let ( >>| ) m f = m >>= fun x -> return (f x)

  let run (m : 'a t) (s : st) : st * 'a = m s

  let default = { env = Map.empty (module String); frame_offset = 0; fresh = 0 }

  let fresh : string t =
    let* st = get in
    let+ _ = put { st with fresh = st.fresh + 1 } in
    "L" ^ Int.to_string st.fresh
  ;;

  let alloc_frame_slot : int t =
    let* st = get in
    let off = st.frame_offset + word_size in
    put { st with frame_offset = off } >>| fun _ -> off
  ;;

  let add_binding name loc : unit t =
    modify (fun st -> { st with env = Map.set st.env ~key:name ~data:loc })
  ;;

  let get_frame_offset : int t =
    let+ st = get in
    st.frame_offset
  ;;

  let set_frame_offset (off : int) : unit t =
    modify (fun st -> { st with frame_offset = off })
  ;;

  let save_var_on_stack name : int t =
    let* off = alloc_frame_slot in
    add_binding name (Stack off) >>| fun _ -> off
  ;;

 let lookup name : location option t = get >>| fun st -> Map.find st.env name
end

open M

let imm_of_const : const -> int = function
  | ConstInt n -> n
  | ConstBool true -> 1
  | ConstBool false -> 0
  | ConstString _ -> failwith "String constants not supported in immediate context"
;;

let gen_immediate dst = function
  | ImmediateConst c ->
    let imm = imm_of_const c in
    M.return [ li dst imm ]
  | ImmediateVar x ->
    let+ loc = M.lookup x in
    (match loc with
     | Some (Stack off) -> [ ld dst (-off) fp ]
     | _ -> failwith ("unbound variable: " ^ x))
;;

let rec gen_complex_expr dst = function
  | ComplexImmediate imm -> gen_immediate dst imm
  | ComplexBranch (c, th, el) ->
    let* cond_code = gen_immediate (T 0) c in
    let* then_code = gen_anf_expr dst th in
    let* else_code = gen_anf_expr dst el in
    let* l_else = M.fresh in
    let+ l_end = M.fresh in
    cond_code
    @ [ beq (T 0) Zero l_else ]
    @ then_code
    @ [ j l_end; label l_else ]
    @ else_code
    @ [ label l_end ]
  | ComplexBinOper (op, e1, e2) ->
    let* c1 = gen_immediate (T 0) e1 in
    let+ c2 = gen_immediate (T 1) e2 in
    (match op with
     | LowestEqual -> c1 @ c2 @ [ slt dst (T 1) (T 0); xori dst dst 1 ]
     | LowerThan -> c1 @ c2 @ [ slt dst (T 0) (T 1) ]
     | GretestEqual -> c1 @ c2 @ [ slt dst (T 0) (T 1); xori dst dst 1 ]
     | GreaterThan -> c1 @ c2 @ [ slt dst (T 1) (T 0) ]
     | Plus -> c1 @ c2 @ [ add dst (T 0) (T 1) ]
     | Minus -> c1 @ c2 @ [ sub dst (T 0) (T 1) ]
     | Multiply -> c1 @ c2 @ [ mul dst (T 0) (T 1) ]
     | Division -> c1 @ c2 @ [ div dst (T 0) (T 1) ]
     | NotEqual -> c1 @ c2 @ [ sub dst (T 0) (T 1); snez dst dst ]
     | Equal -> c1 @ c2 @ [ sub dst (T 0) (T 1); seqz dst dst ]
     | And -> c1 @ c2 @ [ and_ dst (T 0) (T 1) ]
     | Or -> c1 @ c2 @ [ or_ dst (T 0) (T 1) ])
  | ComplexApp (f, arg, args) ->
    (* For simplicity, we handle only single argument applications *)
    let* arg_c = gen_immediate (A 0) arg in
    (match f with
     | ImmediateVar fname ->
       if String.equal fname "exit" then
         (* Handle the exit function specially - it should call the exit syscall *)
         let+ args_c =
           match args with
           | [] -> M.return []
           | _ -> failwith "Multi-argument function calls not implemented yet"
         in
         arg_c @ args_c @ [ mv (A 0) (A 0); li (A 7) 93; ecall ]  (* exit syscall *)
       else
         let+ args_c =
           match args with
           | [] -> M.return []
           | _ -> failwith "Multi-argument function calls not implemented yet"
         in
         arg_c @ args_c @ [ call (show_immediate f) ] @ if dst = A 0 then [] else [ mv dst (A 0) ]
     | _ -> failwith "Function application with non-variable function not implemented")
  | ComplexLambda _ -> failwith "Lambda expressions not implemented yet in code generation"
  | ComplexUnarOper (op, e) ->
    let* e_c = gen_immediate (T 0) e in
    let result =
      match op with
      | Negative -> e_c @ [ li (T 1) (-1); mul dst (T 0) (T 1) ]
      | Not -> e_c @ [ seqz dst (T 0) ]
    in
    M.return result
  | ComplexTuple _ -> failwith "Tuple expressions not implemented yet in code generation"
  | ComplexList _ -> failwith "List expressions not implemented yet in code generation"
  | ComplexOption _ -> failwith "Option expressions not implemented yet in code generation"

and gen_anf_expr dst = function
  | AnfExpr cexpr -> gen_complex_expr dst cexpr
  | AnfLet (NonRec, name, cexpr, body) ->
    let* cexpr_c = gen_complex_expr (T 0) cexpr in
    let* off = save_var_on_stack name in
    let+ body_c = gen_anf_expr dst body in
    cexpr_c @ [ sd (T 0) (-off) fp ] @ body_c
 | AnfLet (Rec, _, _, _) -> failwith "Recursive bindings not implemented yet in code generation"
;;

let gen_anf_structure_item : anf_structure -> instr list M.t = function
  | AnfValue (NonRec, (f, AnfExpr (ComplexLambda ([PatVariable arg], body))), []) ->
    let* saved_off = M.get_frame_offset in
    let* () = M.set_frame_offset 16 in
    let* x_off = save_var_on_stack arg in
    let* body_code = gen_anf_expr (A 0) body in
    let* locals = M.get_frame_offset in
    (* for ra and fp *)
    let frame = locals + (2 * word_size) in
    let+ () = M.set_frame_offset saved_off in
    let prologue =
      [ addi Sp Sp (-frame)
      ; sd Ra (frame - 8) Sp
      ; sd fp (frame - 16) Sp
      ; addi fp Sp frame
      ; sd (A 0) (-x_off) fp
      ]
    in
    let epilogue =
      [ ld Ra (frame - 8) Sp; ld fp (frame - 16) Sp; addi Sp Sp frame; ret ]
    in
    [ label f ] @ prologue @ body_code @ epilogue
  | AnfValue (NonRec, (_name, e), []) ->
    let* body_code = gen_anf_expr (A 0) e in
    let+ frame = M.get_frame_offset in
    [ label "_start"; mv fp Sp; addi Sp Sp (-frame) ]
    @ body_code
    @ [ call "flush"; mv (A 0) (A 0); li (A 7) 93; ecall ]  (* exit syscall *)
  | AnfEval e ->
    let* body_code = gen_anf_expr (A 0) e in
    let+ frame = M.get_frame_offset in
    [ label "_start"; mv fp Sp; addi Sp Sp (-frame) ]
    @ body_code
    @ [ call "flush"; mv (A 0) (A 0); li (A 7) 93; ecall ]  (* exit syscall *)
  | item ->
    failwith (Format.asprintf "not implemented codegen for anf structure item: %a" pp_anf_structure item)
;;

(* Add exit function to handle the exit: int -> 'a requirement *)
let gen_exit_function fmt =
  let open Format in
  fprintf fmt "\nexit:\n";
  fprintf fmt "  li a7, 93\n";  (* exit syscall number *)
  fprintf fmt "  mv a0, a0\n";  (* move exit code from first argument to a0 *)
  fprintf fmt "  ecall\n";
  fprintf fmt "  ret\n"
;;

let rec gather : anf_program -> instr list M.t = function
  | [] -> M.return []
  | item :: rest ->
    let* code1 = gen_anf_structure_item item in
    let+ code2 = gather rest in
    code1 @ code2
;;

let gen_anf_program (pr : anf_program) fmt =
  let open Format in
  fprintf fmt ".text\n";
  fprintf fmt ".globl _start\n";
  let _, code = M.run (gather pr) M.default in
  Base.List.iter code ~f:(function
    | Label l -> fprintf fmt "%s:\n" l
    | i -> fprintf fmt "  %a\n" pp_instr i);
  gen_exit_function fmt
;;