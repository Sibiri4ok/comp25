(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck
open Base
open Gen
open Stdlib

type ident = string [@@deriving eq, show { with_path = false }]

let gen_charc = map Char.chr (int_range (Char.code 'a') (Char.code 'z'))

let is_not_keyword = function
  | "let"
  | "if"
  | "then"
  | "else"
  | "in"
  | "fun"
  | "true"
  | "false"
  | "rec"
  | "and"
  | "function"
  | "match"
  | "with"
  | "type"
  | "of" -> false
  | _ -> true
;;

let gen_id_first_char = QCheck.Gen.oneof_weighted [ 5, char_range 'a' 'z'; 1, return '_' ]
let gen_digit = char_range '0' '9'

let gen_id_char =
  QCheck.Gen.oneof_weighted
    [ 5, gen_id_first_char; 5, char_range 'A' 'Z'; 5, gen_digit; 1, return '\'' ]
;;

let gen_ident =
  let gen_name =
    let* fst = gen_id_first_char >|= Base.Char.to_string in
    let range = if Base.String.( = ) fst "_" then 1 -- 4 else 0 -- 4 in
    let* rest = string_size ~gen:gen_id_char range in
    return (fst ^ rest)
  in
  let rec loop gen =
    gen >>= fun name -> if is_not_keyword name then return name else loop gen
  in
  loop gen_name
;;

module List1 = struct
  type 'a t = 'a * 'a list [@@deriving eq, show { with_path = false }]

  let gen gen_a =
    QCheck.Gen.map
      (fun (gen0, gen1) -> gen0, gen1)
      (QCheck.Gen.pair gen_a (list_size (int_bound 5) gen_a))
  ;;

  let arb gen_a = QCheck.make (gen gen_a)
end

module List2 = struct
  type 'a t = 'a * 'a * 'a list [@@deriving eq, show { with_path = false }]

  let gen gen_a =
    QCheck.Gen.map
      (fun (gen0, gen1, gen2) -> gen0, gen1, gen2)
      (QCheck.Gen.triple gen_a gen_a (QCheck.Gen.list gen_a))
  ;;

  let arb gen_a = QCheck.make (gen gen_a)
end

module Constant = struct
  type t =
    | Const_integer of int (** integer as [52] *)
    | Const_char of char (** char as ['w'] *)
    | Const_string of string (** string as ["Kakadu"] *)
  [@@deriving eq, show { with_path = false }]

  let gen =
    QCheck.Gen.oneof_weighted
      [ 1, QCheck.Gen.map (fun gen0 -> Const_integer gen0) nat_small
      ; 1, QCheck.Gen.map (fun gen0 -> Const_char gen0) gen_charc
      ; 1, QCheck.Gen.map (fun gen0 -> Const_string gen0) (string_small_of gen_charc)
      ]
  ;;

  let arb = QCheck.make gen
end

module TypeExpr = struct
  let gen_ref inner_gen =
    let open QCheck.Gen in
    map ref inner_gen
  ;;

  type level = (int[@gen nat_small]) [@@deriving eq, show { with_path = false }, qcheck]

  type t =
    | Type_arrow of t * t (** Function type [t1 -> t2] *)
    | Type_tuple of t List2.t (** Tuple type [t1 * t2 * ...] *)
    | Type_var of tv ref (** Type variable ['a] *)
    | Quant_type_var of ident (** Quantified type variable ['a. 'a] *)
    | Type_construct of ident * t list (** *)
  [@@deriving eq, show { with_path = false }]

  and tv =
    | Unbound of (ident[@gen gen_ident]) * level (** Free type variable *)
    | Link of (t[@gen gen_sized (n / 2)]) (** Unified type variable *)
  [@@deriving eq, show { with_path = false }]

  let rec gen_sized n =
    match n with
    | 0 -> QCheck.Gen.map (fun gen0 -> Quant_type_var gen0) gen_ident
    | _ ->
      QCheck.Gen.oneof_weighted
        [ 1, QCheck.Gen.map (fun gen0 -> Quant_type_var gen0) gen_ident
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> Type_arrow (gen0, gen1))
              (QCheck.Gen.pair (gen_sized (n / 2)) (gen_sized (n / 2))) )
        ; 1, QCheck.Gen.map (fun gen0 -> Type_tuple gen0) (List2.gen (gen_sized (n / 2)))
        ; 1, QCheck.Gen.map (fun gen0 -> Type_var gen0) (gen_ref (gen_tv_sized (n / 2)))
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> Type_construct (gen0, gen1))
              (QCheck.Gen.pair gen_ident (QCheck.Gen.list (gen_sized (n / 2)))) )
        ]

  and gen_tv_sized n =
    match n with
    | 0 ->
      QCheck.Gen.map
        (fun (gen0, gen1) -> Unbound (gen0, gen1))
        (QCheck.Gen.pair gen_ident gen_level)
    | _ ->
      QCheck.Gen.oneof_weighted
        [ ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> Unbound (gen0, gen1))
              (QCheck.Gen.pair gen_ident gen_level) )
        ; 1, QCheck.Gen.map (fun gen0 -> Link gen0) (gen_sized (n / 2))
        ]
  ;;

  let gen = QCheck.Gen.sized gen_sized
  let gen_tv = QCheck.Gen.sized gen_tv_sized
  let arb_sized n = QCheck.make (gen_sized n)
  let arb_tv_sized n = QCheck.make (gen_tv_sized n)
  let arb = QCheck.make gen
  let arb_tv = QCheck.make gen_tv
end

module Pattern = struct
  type t =
    | Pat_constraint of t * TypeExpr.t (** Pattern [(P : T)] *)
    | Pat_any (** The pattern [_]. *)
    | Pat_var of ident (** A variable pattern such as [x] *)
    | Pat_constant of Constant.t (** Patterns such as [52], ['w'], ["uwu"] *)
    | Pat_tuple of t List2.t (** Patterns [(P1, ..., Pn)]. *)
    | Pat_construct of ident * t option
    (** [Pat_construct(C, args)] represents:
        - [C]               when [args] is [None],
        - [C P]             when [args] is [Some (P)]
        - [C (P1, ..., Pn)] when [args] is
          [Some (Pat_tuple [P1; ...; Pn])] *)
  [@@deriving eq, show { with_path = false }]

  let rec gen_sized n =
    match n with
    | 0 ->
      QCheck.Gen.oneof_weighted
        [ 1, QCheck.Gen.pure Pat_any
        ; 1, QCheck.Gen.map (fun gen0 -> Pat_var gen0) gen_ident
        ; 1, QCheck.Gen.map (fun gen0 -> Pat_constant gen0) Constant.gen
        ]
    | _ ->
      QCheck.Gen.oneof_weighted
        [ 1, QCheck.Gen.pure Pat_any
        ; 1, QCheck.Gen.map (fun gen0 -> Pat_var gen0) gen_ident
        ; 1, QCheck.Gen.map (fun gen0 -> Pat_constant gen0) Constant.gen
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> Pat_constraint (gen0, gen1))
              (QCheck.Gen.pair (gen_sized (n / 2)) (TypeExpr.gen_sized (n / 2))) )
        ; 1, QCheck.Gen.map (fun gen0 -> Pat_tuple gen0) (List2.gen (gen_sized (n / 2)))
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> Pat_construct (gen0, gen1))
              (QCheck.Gen.pair gen_ident (QCheck.Gen.option (gen_sized (n / 2)))) )
        ]
  ;;

  let gen = QCheck.Gen.sized gen_sized
  let arb_sized n = QCheck.make (gen_sized n)
  let arb = QCheck.make gen
end

module Expression = struct
  type rec_flag =
    | Nonrecursive
    | Recursive
  [@@deriving eq, show { with_path = false }]

  let gen_rec_flag =
    QCheck.Gen.oneof_weighted
      [ 1, QCheck.Gen.pure Nonrecursive; 1, QCheck.Gen.pure Recursive ]
  ;;

  let arb_rec_flag = QCheck.make gen_rec_flag

  type 'expr value_binding =
    { pat : Pattern.t
    ; expr : 'expr
    }
  [@@deriving eq, show { with_path = false }]

  let gen_value_binding gen_expr n =
    map2 (fun pat expr -> { pat; expr }) (Pattern.gen_sized (n / 2)) (gen_expr (n / 2))
  ;;

  type 'expr case =
    { first : Pattern.t
    ; second : 'expr
    }
  [@@deriving eq, show { with_path = false }]

  let gen_case gen_expr n =
    map2
      (fun first second -> { first; second })
      (Pattern.gen_sized (n / 2))
      (gen_expr (n / 2))
  ;;

  type t =
    | Exp_ident of ident (** (ident[@gen gen_ident])ifiers such as [x] *)
    | Exp_constant of Constant.t (** Expressions constant such as [1], ['a'], ["true"]**)
    | Exp_tuple of t List2.t (** Expressions [(E1, E2, ..., En)] *)
    | Exp_function of t case List1.t
    (** [Exp_function (P1, [P2; ...; Pn])] represents
        [function P1 | ... | Pn] *)
    | Exp_fun of Pattern.t List1.t * t
    (**[Exp_fun (P1, [P2; ...; Pn], E)] represents:
       [fun P1 ... Pn -> E] *)
    | Exp_apply of t * t
    (** [Pexp_apply(E0, E1)]
                             represents [E0 E1]*)
    | Exp_match of t * t case List1.t (** [match E0 with P1 -> E1 || Pn -> En] *)
    | Exp_constraint of t * TypeExpr.t (** [(E : T)] *)
    | Exp_if of t * t * t option (** [if E1 then E2 else E3] *)
    | Exp_let of rec_flag * t value_binding List1.t * t
    (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
        - [let P1 = E1 and ... and Pn = EN in E]
          when [flag] is [Nonrecursive],
        - [let rec P1 = E1 and ... and Pn = EN in E]
          when [flag] is [Recursive]. *)
    | Exp_construct of ident * t option
    (** [Exp_construct(C, exp)] represents:
        - [C]               when [exp] is [None],
        - [C E]             when [exp] is [Some E],
        - [C (E1, ..., En)] when [exp] is [Some (Exp_tuple[E1;...;En])] *)
  [@@deriving eq, show { with_path = false }]

  let rec gen_sized n =
    match n with
    | 0 ->
      QCheck.Gen.oneof_weighted
        [ 1, QCheck.Gen.map (fun gen0 -> Exp_ident gen0) gen_ident
        ; 1, QCheck.Gen.map (fun gen0 -> Exp_constant gen0) Constant.gen
        ]
    | _ ->
      QCheck.Gen.oneof_weighted
        [ 1, QCheck.Gen.map (fun gen0 -> Exp_ident gen0) gen_ident
        ; 1, QCheck.Gen.map (fun gen0 -> Exp_constant gen0) Constant.gen
        ; 1, QCheck.Gen.map (fun gen0 -> Exp_tuple gen0) (List2.gen (gen_sized (n / 2)))
        ; ( 1
          , QCheck.Gen.map
              (fun gen0 -> Exp_function gen0)
              (List1.gen (gen_case gen_sized (n / 2))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> Exp_fun (gen0, gen1))
              (QCheck.Gen.pair
                 (List1.gen (Pattern.gen_sized (n / 2)))
                 (gen_sized (n / 2))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> Exp_apply (gen0, gen1))
              (QCheck.Gen.pair (gen_sized (n / 2)) (gen_sized (n / 2))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> Exp_match (gen0, gen1))
              (QCheck.Gen.pair
                 (gen_sized (n / 2))
                 (List1.gen (gen_case gen_sized (n / 2)))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> Exp_constraint (gen0, gen1))
              (QCheck.Gen.pair (gen_sized (n / 2)) (TypeExpr.gen_sized (n / 2))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1, gen2) -> Exp_if (gen0, gen1, gen2))
              (QCheck.Gen.triple
                 (gen_sized (n / 2))
                 (gen_sized (n / 2))
                 (QCheck.Gen.option (gen_sized (n / 2)))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1, gen2) -> Exp_let (gen0, gen1, gen2))
              (QCheck.Gen.triple
                 gen_rec_flag
                 (List1.gen (gen_value_binding gen_sized (n / 2)))
                 (gen_sized (n / 2))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> Exp_construct (gen0, gen1))
              (QCheck.Gen.pair gen_ident (QCheck.Gen.option (gen_sized (n / 2)))) )
        ]
  ;;

  let gen = QCheck.Gen.sized gen_sized
  let arb_sized n = QCheck.make (gen_sized n)
  let arb = QCheck.make gen
end

module Structure = struct
  type structure_item =
    | Str_eval of Expression.t
    | Str_value of Expression.rec_flag * Expression.t Expression.value_binding List1.t
    (** [Str_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
        - [let P1 = E1 and ... and Pn = EN]
          when [rec] is [Nonrecursive],
        - [let rec P1 = E1 and ... and Pn = EN ]
          when [rec] is [Recursiv e ee]. *)
  [@@deriving eq, show { with_path = false }]

  let gen_structure_item n =
    oneof_weighted
      [ 1, map (fun expr -> Str_eval expr) (Expression.gen_sized (n / 2))
      ; ( 1
        , let* rec_flag =
            oneof [ return Expression.Nonrecursive; return Expression.Recursive ]
          in
          let* bind1 = Expression.gen_value_binding Expression.gen_sized (n / 2) in
          let* bindl =
            list_small (Expression.gen_value_binding Expression.gen_sized (n / 2))
          in
          return (Str_value (rec_flag, (bind1, bindl))) )
      ]
  ;;

  let arb_structure_item = QCheck.make (gen_structure_item 10)
end

type program = Structure.structure_item list [@@deriving eq, show { with_path = false }]

module Program = struct
  let gen_program n = list_size (int_bound 6) (Structure.gen_structure_item (n / 2))
end
