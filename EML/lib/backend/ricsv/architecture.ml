(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** RISC-V: ISA, platform config, and codegen API in one module. *)

open Base

module Riscv_backend = struct
  (* ----- ISA: types ----- *)
  type reg =
    | Zero
    | RA
    | SP
    | A of int
    | T of int
    | S of int
  [@@deriving eq]

  type offset = reg * int

  type instr =
    | Addi of reg * reg * int
    | Add of reg * reg * reg
    | Sub of reg * reg * reg
    | Mul of reg * reg * reg
    | Srli of reg * reg * int
    | Xori of reg * reg * int
    | Xor of reg * reg * reg
    | Slt of reg * reg * reg
    | Seqz of reg * reg
    | Snez of reg * reg
    | Li of reg * int
    | La of reg * string
    | Mv of reg * reg
    | Ld of reg * offset
    | Sd of reg * offset
    | Beq of reg * reg * string
    | J of string
    | Label of string
    | Call of string
    | Ret

  (* ----- ISA: printing ----- *)
  let pp_reg ppf =
    let open Stdlib.Format in
    function
    | Zero -> fprintf ppf "zero"
    | RA -> fprintf ppf "ra"
    | SP -> fprintf ppf "sp"
    | A n -> fprintf ppf "a%d" n
    | T n -> fprintf ppf "t%d" n
    | S 0 -> fprintf ppf "fp"
    | S n -> fprintf ppf "s%d" n
  ;;

  let pp_offset ppf offset = Stdlib.Format.fprintf ppf "%d(%a)" (snd offset) pp_reg (fst offset)

  let pp_instr ppf =
    let open Stdlib.Format in
    function
    | Addi (rd, rs, imm) -> fprintf ppf "addi %a, %a, %d" pp_reg rd pp_reg rs imm
    | Add (rd, rs1, rs2) -> fprintf ppf "add %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sub (rd, rs1, rs2) -> fprintf ppf "sub %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Mul (rd, rs1, rs2) -> fprintf ppf "mul %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Srli (rd, rs1, imm) -> fprintf ppf "srli %a, %a, %d" pp_reg rd pp_reg rs1 imm
    | Xori (rd, rs1, imm) -> fprintf ppf "xori %a, %a, %d" pp_reg rd pp_reg rs1 imm
    | Xor (rd, rs1, rs2) -> fprintf ppf "xor %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Slt (rd, rs1, rs2) -> fprintf ppf "slt %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Seqz (rd, rs) -> fprintf ppf "seqz %a, %a" pp_reg rd pp_reg rs
    | Snez (rd, rs) -> fprintf ppf "snez %a, %a" pp_reg rd pp_reg rs
    | Li (rd, imm) -> fprintf ppf "li %a, %d" pp_reg rd imm
    | La (rd, s) -> fprintf ppf "la %a, %s" pp_reg rd s
    | Mv (rd, rs) -> fprintf ppf "mv %a, %a" pp_reg rd pp_reg rs
    | Ld (rd, ofs) -> fprintf ppf "ld %a, %a" pp_reg rd pp_offset ofs
    | Sd (rs, ofs) -> fprintf ppf "sd %a, %a" pp_reg rs pp_offset ofs
    | Beq (rs1, rs2, s) -> fprintf ppf "beq %a, %a, %s" pp_reg rs1 pp_reg rs2 s
    | J s -> fprintf ppf "j %s" s
    | Label s -> fprintf ppf "%s:" s
    | Call s -> fprintf ppf "call %s" s
    | Ret -> fprintf ppf "ret"
  ;;

  (* ----- ISA: tag and register names ----- *)
  let tag_int n = 1 + (n lsl 1)
  let fp = S 0
  let sp = SP
  let ra = RA
  let zero = Zero
  let a0 = A 0
  let a1 = A 1
  let a2 = A 2
  let a3 = A 3
  let a4 = A 4
  let a5 = A 5
  let a6 = A 6
  let a7 = A 7
  let t0 = T 0
  let t1 = T 1

  (* ----- Platform (RISC-V layout) ----- *)
  module Platform = struct
    let arg_regs_count = 8
    let word_size = 8
    let frame_header_size = 2 * word_size
    let saved_fp_offset = 0
    let saved_ra_offset = word_size
  end

  let word_size = Platform.word_size
  let arg_regs_count = Platform.arg_regs_count
  let frame_header_size = Platform.frame_header_size
  let saved_ra_offset = Platform.saved_ra_offset
  let saved_fp_offset = Platform.saved_fp_offset
  let result_reg = a0
  let arg_regs = List.init 8 ~f:(fun i -> A i)
  let candidate_regs_for_spill = arg_regs

  let is_caller_saved = function
    | A _ | T _ -> true
    | Zero | RA | SP | S _ -> false
  ;;

  type location =
    | Loc_reg of reg
    | Loc_mem of offset

  type instr_item = instr * string

  let one i comm = [ i, comm ]
  let li ?(comm = "") rd imm = one (Li (rd, imm)) comm
  let la ?(comm = "") rd s = one (La (rd, s)) comm
  let mv ?(comm = "") rd rs = one (Mv (rd, rs)) comm
  let ld ?(comm = "") rd ofs = one (Ld (rd, ofs)) comm
  let sd ?(comm = "") rs ofs = one (Sd (rs, ofs)) comm
  let add ?(comm = "") rd rs1 rs2 = one (Add (rd, rs1, rs2)) comm
  let addi ?(comm = "") rd rs imm = one (Addi (rd, rs, imm)) comm
  let sub ?(comm = "") rd rs1 rs2 = one (Sub (rd, rs1, rs2)) comm
  let mul ?(comm = "") rd rs1 rs2 = one (Mul (rd, rs1, rs2)) comm
  let srli ?(comm = "") rd rs imm = one (Srli (rd, rs, imm)) comm
  let xori ?(comm = "") rd rs imm = one (Xori (rd, rs, imm)) comm
  let xor ?(comm = "") rd rs1 rs2 = one (Xor (rd, rs1, rs2)) comm
  let slt ?(comm = "") rd rs1 rs2 = one (Slt (rd, rs1, rs2)) comm
  let seqz ?(comm = "") rd rs = one (Seqz (rd, rs)) comm
  let snez ?(comm = "") rd rs = one (Snez (rd, rs)) comm
  let beq ?(comm = "") rs1 rs2 lbl = one (Beq (rs1, rs2, lbl)) comm
  let j ?(comm = "") lbl = one (J lbl) comm
  let label ?(comm = "") s = one (Label s) comm
  let call ?(comm = "") s = one (Call s) comm
  let ret ?(comm = "") () = one Ret comm
  let add_tag_items ?(comm = "") dst delta = one (Addi (dst, dst, delta)) comm

  let compare_ordering dst r1 r2 ~invert =
    let base = slt dst r1 r2 in
    if invert then base @ xori dst dst 1 else base
  ;;

  let compare_eq_ne dst r1 r2 ~is_eq =
    let base = xor dst r1 r2 in
    if is_eq then base @ seqz dst dst else base @ snez dst dst
  ;;

  let bin_op dst op r1 r2 =
    match op with
    | "+" -> add dst r1 r2 @ add_tag_items dst (-1)
    | "-" -> sub dst r1 r2 @ add_tag_items dst 1
    | "*" -> srli r1 r1 1 @ addi r2 r2 (-1) @ mul dst r1 r2 @ add_tag_items dst 1
    | "<" -> compare_ordering dst r1 r2 ~invert:false
    | ">" -> compare_ordering dst r2 r1 ~invert:false
    | "<=" -> compare_ordering dst r2 r1 ~invert:true
    | ">=" -> compare_ordering dst r1 r2 ~invert:true
    | "=" -> compare_eq_ne dst r1 r2 ~is_eq:true
    | "<>" -> compare_eq_ne dst r1 r2 ~is_eq:false
    | _ -> failwith ("unsupported binary operator: " ^ op)
  ;;

  let prologue ~name ~stack_size =
    let ra_slot = SP, stack_size - saved_ra_offset in
    let fp_slot = SP, stack_size - frame_header_size in
    label name
    @ addi sp sp (-stack_size)
    @ sd ra ra_slot
    @ sd fp fp_slot
    @ addi ~comm:"Prologue ends" fp sp (stack_size - frame_header_size)
  ;;

  let epilogue ~is_main =
    let base =
      addi sp fp frame_header_size ~comm:"Epilogue starts"
      @ ld ra (fp, saved_ra_offset)
      @ ld fp (fp, saved_fp_offset)
    in
    if is_main then base @ li a0 0 @ ret () else base @ ret ()
  ;;

  let load_reg (dst_reg : reg) = function
    | Loc_reg src_reg when equal_reg src_reg dst_reg -> []
    | Loc_reg src_reg -> mv dst_reg src_reg
    | Loc_mem ofs -> ld dst_reg ofs
  ;;

  let format_item ppf i =
    (match i with
     | Label _ -> Stdlib.Format.fprintf ppf "%a" pp_instr i
     | _ -> Stdlib.Format.fprintf ppf "  %a" pp_instr i);
    Stdlib.Format.fprintf ppf "\n"
  ;;
end
