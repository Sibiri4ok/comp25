(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Pprinter
open Common.Ast
open Common.Parser
open Format

let parse_prog str = Angstrom.parse_string ~consume:Angstrom.Consume.All pstructure str

let print_prog_with_ast prog =
  let parsed =     (parse_prog (asprintf "%a" pprint_program prog)) in
  asprintf
    "AST:\n\n%s\n\nPprinted:\n\n%s\n\nParsed:\n\n%s\n\nParsed Pprinted:\n\n%s\n\n"
    (show_program prog)
    (asprintf "%a" pprint_program prog)
    (show_program (((fun r ->
      if Result.is_ok r
        then Result.get_ok r
    else (printf "Result is not ok, printing initial prog\n";
    prog))
    parsed )))
    (asprintf "%a" pprint_program ((fun r ->
      if Result.is_ok r
        then Result.get_ok r
    else (printf "Result is not ok, printing initial prog\n";
    prog))
    parsed))
;;

let arb_program =
    QCheck.make
      ~print:print_prog_with_ast
      (QCheck.Gen.sized Program.gen_program)

let run n =
  QCheck_base_runner.run_tests
    [ QCheck.(
        Test.make arb_program ~count:n (fun pr ->
          let res = parse_str (asprintf "%a\n" pprint_program  pr) in
          pr = res))
    ]
;;

let run_tests n =
  let _ = run n in
  ()
;;

let () =
  Arg.parse
    [ "-seed", Arg.Int QCheck_base_runner.set_seed, " Set seed"
    ; "-stop", Arg.Unit (fun _ -> exit 0), " Exit"
    ; "-gen", Arg.Int run_tests, " Exit"
    ]
    (fun _ ->  ())
    "help"
;;