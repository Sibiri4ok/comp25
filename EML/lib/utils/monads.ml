(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ANFMonad = struct
  type 'a t = int -> int * ('a, string) Result.t

  let return x = fun counter -> counter, Ok x

  let ( >>= ) m f =
    fun counter ->
    match m counter with
    | counter', Ok a -> f a counter'
    | counter', Error e -> counter', Error e
  ;;

  let fresh : string t = fun counter -> counter + 1, Ok ("anf_t" ^ Int.to_string counter)
  let run m = m 0 |> snd
  let fail msg = fun counter -> counter, Error msg

  (** Монадические операторы *)
  module Syntax = struct
    let ( let* ) = ( >>= )
  end
end
