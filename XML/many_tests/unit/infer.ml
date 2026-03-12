(** Copyright 2025-2026, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Infer
open Common.Parser
open Common.Pprinter
open Common.Ast.TypeExpr

let infer_exp_str ?(rst = true) ?(env = []) str =
  let exp = parse_exp_str str in
  if rst then reset_gensym ();
  match infer_exp env exp with
  | Ok (_, ty) ->
  pprint_type Format.std_formatter ty
  | Error err -> pprint_err Format.std_formatter err

let infer_pat_str ?(rst = true) ?(env = []) str =
  let pat = parse_pat_str str in
  if rst then reset_gensym ();
    match infer_pat env pat  with
  | Ok (_, ty) ->
  pprint_type Format.std_formatter ty
  | Error err -> pprint_err Format.std_formatter err

let infer_prog_str ?(env = env_with_things) str =
  let prog = parse_str str in
  reset_gensym ();
  match infer_program env prog with
    | Ok (new_env, names) ->
  pprint_env new_env names
  | Error err -> pprint_err Format.std_formatter err

let show_etyp env exp =
  match infer_exp env exp with
  | Ok (_, ty) ->
  Stdio.print_endline (Common.Ast.TypeExpr.show ty)
  | Error err -> pprint_err Format.std_formatter err

let type_bool = Type_construct ("bool", [])
let type_unit = Type_construct ("unit", [])
let type_char = Type_construct ("char", [])
let type_int = Type_construct ("int", [])
let type_string = Type_construct ("string", [])

let env = env_with_things

(************************** Expressions **************************)

let%expect_test "char" =
  infer_exp_str {| 'a' |};
  [%expect{| char |}]


let%expect_test "int" =
  infer_exp_str {| 1 |};
  [%expect{| int |}]


let%expect_test "str" =
  infer_exp_str {| "Kakadu" |};
  [%expect{| string |}]


let%expect_test "id in env" =
  infer_exp_str {| m |} ~env:[("m", Type_var {contents = Unbound ("a", 0)})];
  [%expect{| 'a |}]


let%expect_test "id not in env" =
  infer_exp_str {| m |};
 [%expect{| Unbound variable m |}];;


let%expect_test "tuple 2" =
  infer_exp_str {| (1, 2) |};
  [%expect{| int * int |}]


let%expect_test "tuple 3" =
  infer_exp_str {| (1, 2, 3) |};
  [%expect{| int * int * int |}]


  let%expect_test "tuple 4" =
  infer_exp_str {| (1, 2, 3, 4) |};
  [%expect{| int * int * int * int |}]


let%expect_test "tuples in tuple" =
  infer_exp_str {| ((1, 2), (3, 4)) |};
  [%expect{| (int * int) * (int * int) |}]


 let%expect_test "construct none" =
  infer_exp_str {| None |} ~env;
 [%expect {| 'a option |}]


let%expect_test "construct some" =
  infer_exp_str {| Some 1 |} ~env;
 [%expect {| int option |}]


let%expect_test "if (string) " =
  infer_exp_str {| if "trololo" then 1 |};
  [%expect {| Cannot unify different constructors: string and bool |}]


let%expect_test "if (bool) then (not unit)" =
  let env = ["cond", type_bool] in
  infer_exp_str {| if cond then 1 |} ~env;
  [%expect{| Cannot unify different constructors: int and unit |}]


let%expect_test "if (bool) then (unit)" =
  let env = ["cond", type_bool; "bodyvar", type_unit] in
  infer_exp_str {| if cond then bodyvar |} ~env;
  [%expect{| unit |}]


let%expect_test "if (bool) then 'a else 'a" =
  let env = ["cond", type_bool; "x", Type_var {contents = Unbound ("a", 0)}; "y", Type_var {contents = Unbound ("a", 0)}] in
  infer_exp_str  {| if cond then x else y |}  ~env;
  [%expect{| 'a |}]


let%expect_test "if (bool) then 'a else 'b" =
  let env = ["cond", type_bool; "x", Type_var {contents = Unbound ("a", 0)}; "y", Type_var {contents = Unbound ("b", 0)}] in
  infer_exp_str  {| if cond then x else y |}  ~env;
  [%expect{| 'b |}]


let%expect_test "apply int -> int to int" =
  let env = ["f", Type_arrow (type_int, type_int); "x", type_int] in
  infer_exp_str  {| f x |} ~env;
  [%expect{| int |}]


let%expect_test "apply int -> int to string" =
  let env = ["f", Type_arrow (type_int, type_int); "x", type_string] in
  infer_exp_str {| f x |} ~env;
  [%expect{| Cannot unify different constructors: int and string |}]


let%expect_test "apply 'a -> 'a to 'b" =
  let env = ["f", Type_arrow (Type_var {contents = Unbound ("s", 0)}, Type_var {contents = Unbound ("s", 0)}); "x", Type_var {contents = Unbound ("t", 0)}] in
  infer_exp_str {| f x |} ~env ~rst: false;
  [%expect{| 'b |}]


  (* not sure if this is right *)
let%expect_test "apply 'a to 'a (different vars)" =
  let env = ["f", Type_var {contents = Unbound ("t", 0)}; "x", Type_var {contents = Unbound ("t", 0)}] in
  infer_exp_str {| f x |} ~env ~rst: false;
  [%expect {| 'c |}]


let%expect_test "apply 'a to 'a (same var)" =
  let env = ["x", Type_var {contents = Unbound ("t", 0)}] in
  infer_exp_str {| x x |} ~env ~rst: false;
  [%expect{| Occurs check |}]


let%expect_test "apply 'a to 'b" =
  let env = ["f", Type_var {contents = Unbound ("s", 0)}; "x", Type_var {contents = Unbound ("t", 0)}] in
  infer_exp_str {| f x |} ~env ~rst: false;
  [%expect{| 'e |}]


(************************** Patterns **************************)

let%expect_test "id in env" =
  let env = ["m", (Type_var {contents = Unbound ("c", 0)})] in
  infer_pat_str {| m |} ~env;
  [%expect {| 'a |}];;


let%expect_test "id not in env" =
  infer_pat_str {| m |};
  [%expect {| 'a |}];;


let%expect_test "any" =
  infer_pat_str {| _ |};
  [%expect {| 'a |}];;


let%expect_test "char" =
  infer_pat_str {| 'a' |};
  [%expect {| char |}];;


let%expect_test "int" =
  infer_pat_str {| 1 |};
  [%expect {| int |}];;


let%expect_test "str" =
  infer_pat_str {| "kakadu" |};
  [%expect {| string |}];;


let%expect_test "tuple 2" =
  infer_pat_str {| (1, 2) |};
  [%expect {| int * int |}];;


let%expect_test "tuple 3" =
  infer_pat_str {| (1, 2, 3) |};
  [%expect {| int * int * int |}];;


let%expect_test "tuple 4" =
  infer_pat_str {| (1, 2, 3, 4) |};
   [%expect {| int * int * int * int |}];;


let%expect_test "tuples in tuple" =
  infer_pat_str {| ((1, 2), (3, 4)) |};
   [%expect {| (int * int) * (int * int) |}];;


let%expect_test "construct none" =
  infer_pat_str {| None |} ~env;
  [%expect {| 'a option |}]


 let%expect_test "construct some" =
  infer_pat_str {| Some 1 |} ~env;
 [%expect{| int option |}]


(************************** Funs **************************)

let%expect_test "fun 'a -> 'a (new var)" =
  infer_exp_str {| fun x -> x |};
  [%expect {| 'a -> 'a |}]


let%expect_test "fun 'a -> 'a (shadow)" =
  let env = ["x", Type_var {contents = Unbound ("s", 0)}] in
  infer_exp_str {| fun x -> x |} ~env;
  [%expect {| 'a -> 'a |}]


let%expect_test "fun 'a -> 'b (not in env)" =
  infer_exp_str {| fun x -> y |};
  [%expect{| Unbound variable y |}]


let%expect_test "fun 'a -> 'b (in env)" =
  let env = ["y", Type_var {contents = Unbound ("s", 0)}] in
  infer_exp_str {| fun x -> y |} ~env;
  [%expect{| 'a -> 's |}]


let%expect_test {| fun x -> fun y -> x y |} =
  infer_exp_str {| fun x -> fun y -> x y |};
 [%expect{| ('b -> 'c) -> 'b -> 'c |}]


let%expect_test {| fun x y -> x y |} =
  infer_exp_str {| fun x y -> x y |};
  [%expect{| ('b -> 'c) -> 'b -> 'c |}]


let%expect_test {| (fun f a b -> f a, f b) (fun x -> x) 1 "mystr" |}  =
  infer_exp_str {| (fun f a b -> f a, f b) (fun x -> x) 1 "mystr" |};
  [%expect{| Cannot unify different constructors: int and string |}]


(************************** Match, function **************************)

let%expect_test "correct match" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound  ("a", 0)}])] @ env in
  infer_exp_str {| match a with | Some x -> 1 | None -> 2 |} ~env;
  [%expect {|
    int |}]


let%expect_test "use match pattern in body" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound ("a", 0)}])] @ env in
  infer_exp_str {| match a with | Some x -> x | None -> 2 |} ~env;
  [%expect {|
    int |}]


let%expect_test "match different constructors" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound ("a", 0)}])] @ env in
  infer_exp_str {| match a with | Some x -> 1 | [] -> 2 |} ~env;
  [%expect{| Cannot unify different constructors: list and option |}]


let%expect_test "match option with list constructors" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound ("a", 0)}])] @ env in
  infer_exp_str {| match a with | x :: tl -> 1 | [] -> 2 |} ~env;
  [%expect{| Cannot unify different constructors: list and option |}]


let%expect_test "match different types of expr 1" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound ("a", 0)}])] @ env in
  infer_exp_str {| match a with | Some x -> 'a' | None -> 1234 |} ~env;
  [%expect{| Cannot unify different constructors: char and int |}]


let%expect_test "match different types of expr 2" =
  let env = [ "b", Type_construct("list", [Type_var {contents = Unbound ("a", 0)}])] @ env in
  infer_exp_str {| match b with | x :: y :: tl -> 'a' | x :: tl -> 'b' | _ -> 1234 |} ~env;
  [%expect{| Cannot unify different constructors: char and int |}]


let%expect_test "correct function" =
  infer_exp_str {| function | Some x -> 1 | None -> 2 |} ~env;
  [%expect {|
    'd option -> int |}]


let%expect_test "use function pattern in body" =
  infer_exp_str {| function | Some x -> x | None -> 2 |} ~env;
  [%expect {|
    int option -> int |}]


let%expect_test "function different constructors" =
  infer_exp_str {| function | Some x -> 1 | [] -> 2 |} ~env;
  [%expect{| Cannot unify different constructors: list and option |}]


let%expect_test "function different types of expr 1" =
  infer_exp_str {| function | Some x -> 'a' | None -> 1234 |} ~env;
  [%expect{| Cannot unify different constructors: char and int |}]


let%expect_test "function different types of expr 2" =
  infer_exp_str {| function | x :: y :: tl -> 'a' | x :: tl -> 'b' | _ -> 1234 |} ~env;
  [%expect{| Cannot unify different constructors: char and int |}]


(************************** Let in **************************)

let%expect_test {| let 1 = 1 in 2 |}  =
  infer_exp_str {| let 1 = 1 in 2 |};
  [%expect{| int |}]


let%expect_test {| let a = 1 in 2 |} =
  infer_exp_str {| let a = 1 in 2 |};
  [%expect{| int |}]


let%expect_test {| let a = 1 in a |} =
infer_exp_str {| let a = 1 in a |};
  [%expect{| int |}]


let%expect_test {| let a = 1 in "str" |} =
  infer_exp_str {| let a = 1 in "str" |};
  [%expect{| string |}]


let%expect_test "let poly" =
  infer_exp_str {| let a = fun x -> x in a |};
  [%expect {| 'c -> 'c |}]


let%expect_test "let poly 2" =
  infer_exp_str {| let a = fun x -> x in (a 1, a "str") |};
  [%expect{| int * string |}]


let%expect_test "poly in env" =
  let env = ["=", Type_arrow (Quant_type_var "a", Type_arrow(Quant_type_var "a", type_bool))] in
  infer_exp_str {| let a = 1 in 1 = 1, "str" = "str" |} ~env;
  [%expect{| bool * bool |}]


let%expect_test {| let a, b = 1, 2 in a |} =
  infer_exp_str  {| let a, b = 1, 2 in a |} ;
  [%expect{| int |}]


let%expect_test {| let a, b, c = 1, 2 in a |} =
  infer_exp_str  {| let a, b, c = 1, 2 in a |} ;
  [%expect{| Cannot unify tuples of different sizes |}]


let%expect_test {| let a, b = 1, 2, 3 in a |} =
  infer_exp_str {| let a, b = 1, 2, 3 in a |};
  [%expect{| Cannot unify tuples of different sizes |}]


let%expect_test "let and" =
  infer_exp_str {| let a = 1 and b = "punk" in b |};
  [%expect {| string |}]


let%expect_test "factorial" =
  infer_exp_str {| let rec fac n = if n = 1 then 1 else n * fac (n-1) in fac 4 |} ~env;
  [%expect {| int |}]


let%expect_test "fibonacci" =
  infer_exp_str {| let rec fib n = if n <= 1 then 1 else (fib (n-1)) + (fib (n-2)) in fib 4 |} ~env;
  [%expect {| int |}]


let%expect_test "mutual recursion" =
  let env = env @ ["not", Type_arrow (type_bool, type_bool); "true", type_bool] in
  infer_exp_str ~env {|
  let rec is_odd n =
    if n = 0 then true else is_even (n-1)
  and is_even n = not (is_odd n)
    in is_odd 5
  |};
  [%expect {| bool |}]


let%expect_test "shadow with itself" =
  infer_prog_str 
  {|
  let test3 a b c =
    let a = print_int a in 0
  |};
[%expect{| val test3 : int -> 'a -> 'b -> int |}];;


let%expect_test "shadow" =
  infer_prog_str {|
    let x = 5
    
    let x = "string"
  |};
[%expect {|
  val x : string |}]


let%expect_test "shadow with itself 2" =
  infer_prog_str {|
    let x = 5
    
    let x = x
  |};
[%expect {|
  val x : int |}]


let%expect_test "weird let rec" =
  infer_prog_str {| let rec x = x |};
[%expect{| This kind of expression is not allowed as right-hand side of `let rec' |}]


let%expect_test "too polymorphic1" =
  infer_prog_str {| let map f p = let (a,b) = p in (f a, f b) |};
[%expect {| val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b |}]
