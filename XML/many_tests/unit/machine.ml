(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Backend.Machine

let test x =
  if x then Format.fprintf Format.std_formatter "true" else Format.fprintf Format.std_formatter "false"

let%expect_test "addi" =
  test (Addi (SP, SP, 8) = Addi (SP, SP, 8));
  [%expect {| true |}]

let%expect_test "addi false" =
  test (Addi (SP, SP, 8) = Addi (SP, SP, 16));
  [%expect {| false |}]

let%expect_test "add" =
  test (Add (SP, SP, A 0) = Add (SP, SP, A 0));
  [%expect {| true |}]

let%expect_test "add false" =
  test (Add (SP, SP, A 0) = Add (SP, A 0, SP));
  [%expect {| false |}]

let%expect_test "sub" =
  test (Sub (SP, SP, A 0) = Sub (SP, SP, A 0));
  [%expect {| true |}]

let%expect_test "sub false" =
  test (Sub (SP, SP, A 0) = Sub (SP, A 0, SP));
  [%expect {| false |}]

let%expect_test "mul" =
  test (Mul (SP, SP, A 0) = Mul (SP, SP, A 0));
  [%expect {| true |}]

let%expect_test "mul false" =
  test (Mul (SP, SP, A 0) = Mul (SP, A 0, SP));
  [%expect {| false |}]

let%expect_test "slt" =
  test (Slt (SP, SP, A 0) = Slt (SP, SP, A 0));
  [%expect {| true |}]

let%expect_test "slt false" =
  test (Slt (SP, SP, A 0) = Slt (SP, A 0, SP));
  [%expect {| false |}]

let%expect_test "seqz" =
  test (Seqz (SP, A 0) = Seqz (SP, A 0));
  [%expect {| true |}]

let%expect_test "seqz false" =
  test (Seqz (SP, A 0) = Seqz (A 0, SP));
  [%expect {| false |}]

let%expect_test "snez" =
  test (Snez (SP, A 0) = Snez (SP, A 0));
  [%expect {| true |}]

let%expect_test "snez false" =
  test (Snez (SP, A 0) = Snez (A 0, SP));
  [%expect {| false |}]

let%expect_test "xor" =
  test (Xor (SP, SP, A 0) = Xor (SP, SP, A 0));
  [%expect {| true |}]

let%expect_test "xor false" =
  test (Xor (SP, SP, A 0) = Xor (SP, A 0, SP));
  [%expect {| false |}]

let%expect_test "xori" =
  test (Xori (SP, SP, 8) = Xori (SP, SP, 8));
  [%expect {| true |}]

let%expect_test "xori false" =
  test (Xori (SP, SP, 8) = Xori (SP, SP, 16));
  [%expect {| false |}]

let%expect_test "beq" =
  test (Beq (SP, A 0, "label1") = Beq (SP, A 0, "label1"));
  [%expect {| true |}]

let%expect_test "beq false reg" =
  test (Beq (SP, A 0, "label1") = Beq (A 0, SP, "label1"));
  [%expect {| false |}]

let%expect_test "beq false label" =
  test (Beq (SP, A 0, "label1") = Beq (SP, A 0, "label2"));
  [%expect {| false |}]

let%expect_test "blt" =
  test (Blt (SP, A 0, "label1") = Blt (SP, A 0, "label1"));
  [%expect {| true |}]

let%expect_test "blt false reg" =
  test (Blt (SP, A 0, "label1") = Blt (A 0, SP, "label1"));
  [%expect {| false |}]

let%expect_test "blt false label" =
  test (Blt (SP, A 0, "label1") = Blt (SP, A 0, "label2"));
  [%expect {| false |}]

let%expect_test "ble" =
  test (Ble (SP, A 0, "label1") = Ble (SP, A 0, "label1"));
  [%expect {| true |}]

let%expect_test "ble false reg" =
  test (Ble (SP, A 0, "label1") = Ble (A 0, SP, "label1"));
  [%expect {| false |}]

let%expect_test "ble false label" =
  test (Ble (SP, A 0, "label1") = Ble (SP, A 0, "label2"));
  [%expect {| false |}]

let%expect_test "lla" =
  test (Lla (SP, "label1") = Lla (SP, "label1"));
  [%expect {| true |}]

let%expect_test "lla false reg" =
  test (Lla (SP, "label1") = Lla (A 0, "label1"));
  [%expect {| false |}]

let%expect_test "lla false label" =
  test (Lla (SP, "label1") = Lla (SP, "label2"));
  [%expect {| false |}]

let%expect_test "li" =
  test (Li (SP, 42) = Li (SP, 42));
  [%expect {| true |}]

let%expect_test "li false reg" =
  test (Li (SP, 42) = Li (A 0, 42));
  [%expect {| false |}]

let%expect_test "li false imm" =
  test (Li (SP, 42) = Li (SP, 100));
  [%expect {| false |}]

let%expect_test "ld" =
  test (Ld (SP,  (SP, 8)) = Ld (SP,  (SP, 8)));
  [%expect {| true |}]

let%expect_test "ld false reg" =
  test (Ld (SP,  (SP, 8)) = Ld (A 0,  (SP, 8)));
  [%expect {| false |}]

let%expect_test "ld false  reg" =
  test (Ld (SP,  (SP, 8)) = Ld (SP,  (A 0, 8)));
  [%expect {| false |}]

let%expect_test "ld false  imm" =
  test (Ld (SP,  (SP, 8)) = Ld (SP,  (SP, 16)));
  [%expect {| false |}]

let%expect_test "sd" =
  test (Sd (SP,  (SP, 8)) = Sd (SP,  (SP, 8)));
  [%expect {| true |}]

let%expect_test "sd false reg" =
  test (Sd (SP,  (SP, 8)) = Sd (A 0,  (SP, 8)));
  [%expect {| false |}]

let%expect_test "sd false  reg" =
  test (Sd (SP,  (SP, 8)) = Sd (SP,  (A 0, 8)));
  [%expect {| false |}]

let%expect_test "sd false  imm" =
  test (Sd (SP,  (SP, 8)) = Sd (SP,  (SP, 16)));
  [%expect {| false |}]

let%expect_test "mv" =
  test (Mv (SP, A 0) = Mv (SP, A 0));
  [%expect {| true |}]

let%expect_test "mv false" =
  test (Mv (SP, A 0) = Mv (A 0, SP));
  [%expect {| false |}]

let%expect_test "comment" =
  test (Comment "test comment" = Comment "test comment");
  [%expect {| true |}]

let%expect_test "comment false" =
  test (Comment "test comment" = Comment "different comment");
  [%expect {| false |}]

let%expect_test "label" =
  test (Label "label1" = Label "label1");
  [%expect {| true |}]

let%expect_test "label false" =
  test (Label "label1" = Label "label2");
  [%expect {| false |}]

let%expect_test "call" =
  test (Call "func1" = Call "func1");
  [%expect {| true |}]

let%expect_test "call false" =
  test (Call "func1" = Call "func2");
  [%expect {| false |}]

let%expect_test "j" =
  test (J "label1" = J "label1");
  [%expect {| true |}]

let%expect_test "j false" =
  test (J "label1" = J "label2");
  [%expect {| false |}]

let%expect_test "ecall" =
  test (Ecall = Ecall);
  [%expect {| true |}]

let%expect_test "ecall false" =
  test (Ecall = Ret);
  [%expect {| false |}]

let%expect_test "ret" =
  test (Ret = Ret);
  [%expect {| true |}]

let%expect_test "ret false" =
  test (Ret = Ecall);
  [%expect {| false |}]

let%expect_test "la" =
  test (La (SP, "func1") = La (SP, "func1"));
  [%expect {| true |}]

let%expect_test "la false reg" =
  test (La (SP, "func1") = La (A 0, "func1"));
  [%expect {| false |}]

let%expect_test "la false label" =
  test (La (SP, "func1") = La (SP, "func2"));
  [%expect {| false |}]

let%expect_test "slli" =
  test (Slli (SP, A 0, 4) = Slli (SP, A 0, 4));
  [%expect {| true |}]

let%expect_test "slli false reg dest" =
  test (Slli (SP, A 0, 4) = Slli (A 0, A 0, 4));
  [%expect {| false |}]

let%expect_test "slli false reg src" =
  test (Slli (SP, A 0, 4) = Slli (SP, SP, 4));
  [%expect {| false |}]

let%expect_test "slli false imm" =
  test (Slli (SP, A 0, 4) = Slli (SP, A 0, 8));
  [%expect {| false |}]

let%expect_test "srai" =
  test (Srai (SP, A 0, 4) = Srai (SP, A 0, 4));
  [%expect {| true |}]

let%expect_test "srai false reg dest" =
  test (Srai (SP, A 0, 4) = Srai (A 0, A 0, 4));
  [%expect {| false |}]

let%expect_test "srai false reg src" =
  test (Srai (SP, A 0, 4) = Srai (SP, SP, 4));
  [%expect {| false |}]

let%expect_test "srai false imm" =
  test (Srai (SP, A 0, 4) = Srai (SP, A 0, 8));
  [%expect {| false |}]