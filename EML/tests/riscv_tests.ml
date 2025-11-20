open EML_lib.Frontend.Ast
open EML_lib.Middleend.Anf
open Backend.Riscv

let string_contains str substr =
  let str_len = String.length str in
 let substr_len = String.length substr in
 let rec check i =
    if i + substr_len > str_len then false
    else if String.sub str i substr_len = substr then true
    else check (i + 1)
  in
 check 0

let test_simple_program () =
  (* Create a simple program: let x = 42 in x *)
  let const_expr = ExpConst (ConstInt 42) in
  let prog = [SValue (NonRec, (PatVariable "x", const_expr), [])] in
  
  (* Convert to ANF *)
  match anf_program prog with
  | Ok anf_prog ->
    (* Generate RISC-V code *)
    let output = Buffer.create 256 in
    let formatter = Format.formatter_of_buffer output in
    gen_anf_program anf_prog formatter;
    Format.pp_print_flush formatter ();
    let riscv_code = Buffer.contents output in
    
    (* Check that the code contains expected elements *)
    assert (string_contains riscv_code ".text");
    assert (string_contains riscv_code ".globl _start");
    assert (string_contains riscv_code "_start:");
    
    Printf.printf "Generated RISC-V code:\n%s\n" riscv_code;
    true
  | Error msg ->
    Printf.printf "ANF conversion failed: %s\n" msg;
    false

let test_exit_function () =
  (* Test that the exit function is generated *)
  let const_expr = ExpConst (ConstInt 0) in
  let prog = [SValue (NonRec, (PatVariable "main", const_expr), [])] in
  
  match anf_program prog with
 | Ok anf_prog ->
    let output = Buffer.create 256 in
    let formatter = Format.formatter_of_buffer output in
    gen_anf_program anf_prog formatter;
    Format.pp_print_flush formatter ();
    let riscv_code = Buffer.contents output in
    
    (* Check that the exit function is present *)
    assert (string_contains riscv_code "exit:");
    assert (string_contains riscv_code "li a7, 93");
    
    Printf.printf "Generated RISC-V code with exit function:\n%s\n" riscv_code;
    true
  | Error msg ->
    Printf.printf "ANF conversion failed: %s\n" msg;
    false

let%test "simple_program_test" = test_simple_program ()
let%test "exit_function_test" = test_exit_function ()

let string_contains str substr =
  let str_len = String.length str in
 let substr_len = String.length substr in
 let rec check i =
    if i + substr_len > str_len then false
    else if String.sub str i substr_len = substr then true
    else check (i + 1)
  in
 check 0

let test_simple_program () =
  (* Create a simple program: let x = 42 in x *)
  let const_expr = ExpConst (ConstInt 42) in
  let prog = [SValue (NonRec, (PatVariable "x", const_expr), [])] in
  
  (* Convert to ANF *)
  match anf_program prog with
  | Ok anf_prog ->
    (* Generate RISC-V code *)
    let output = Buffer.create 256 in
    let formatter = Format.formatter_of_buffer output in
    gen_anf_program anf_prog formatter;
    Format.pp_print_flush formatter ();
    let riscv_code = Buffer.contents output in
    
    (* Check that the code contains expected elements *)
    assert (string_contains riscv_code ".text");
    assert (string_contains riscv_code ".globl _start");
    assert (string_contains riscv_code "_start:");
    
    Printf.printf "Generated RISC-V code:\n%s\n" riscv_code;
    true
  | Error msg ->
    Printf.printf "ANF conversion failed: %s\n" msg;
    false

let test_exit_function () =
  (* Test that the exit function is generated *)
  let const_expr = ExpConst (ConstInt 0) in
  let prog = [SValue (NonRec, (PatVariable "main", const_expr), [])] in
  
  match anf_program prog with
  | Ok anf_prog ->
    let output = Buffer.create 256 in
    let formatter = Format.formatter_of_buffer output in
    gen_anf_program anf_prog formatter;
    Format.pp_print_flush formatter ();
    let riscv_code = Buffer.contents output in
    
    (* Check that the exit function is present *)
    assert (string_contains riscv_code "exit:");
    assert (string_contains riscv_code "li a7, 93");
    
    Printf.printf "Generated RISC-V code with exit function:\n%s\n" riscv_code;
    true
  | Error msg ->
    Printf.printf "ANF conversion failed: %s\n" msg;
    false

let () =
  Printf.printf "Running RISC-V backend tests...\n";
  let test1_result = test_simple_program () in
  let test2_result = test_exit_function () in
  if test1_result && test2_result then
    Printf.printf "All tests passed!\n"
  else
    Printf.printf "Some tests failed!\n"