(** Copyright 2026, Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast

type error

val pprint_err : Format.formatter -> error -> unit

(** resets counter for type variables *)
val reset_gensym : unit -> unit

(** [infer_exp env exp] infers type of the expression [exp] in the environment [env] and returns
  updated environment and type of [exp] *)
val infer_exp
  :  (ident * TypeExpr.t) list
  -> Expression.t
  -> ((ident * TypeExpr.t) list * TypeExpr.t, error) result

(** [infer_pat env pat] infers type of the pattern [pat] in the environment [env] and returns
  updated environment and type of [pat] *)
val infer_pat
  :  (ident * TypeExpr.t) list
  -> Pattern.t
  -> ((ident * TypeExpr.t) list * TypeExpr.t, error) result

(** [infer_structure_item env item] infers type of the item [item] in the environment [env] and returns
  updated environment and new names *)
val infer_structure_item
  :  (ident * TypeExpr.t) list
  -> Structure.structure_item
  -> ((ident * TypeExpr.t) list * ident list, error) result

(** [infer_program env prog] infers all types in program [prog] with initial environment [env] and returns
  updated environment and names of all new global identificators

  for basic environment, use [env_with_things] *)
val infer_program
  :  (ident * TypeExpr.t) list
  -> Structure.structure_item list
  -> ((ident * TypeExpr.t) list * ident list, error) result

(** [env_with_things] is the basic environment that contains built-in functions and constructors *)
val env_with_things : (ident * TypeExpr.t) list
