(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Backend.Machine
open QCheck.Gen

let arb_machine_instrs =
    QCheck.make
      (QCheck.Gen.list_size (5--20) gen_instr)

let run n =
  QCheck_base_runner.run_tests
    [ QCheck.(
        Test.make arb_machine_instrs ~count:n (fun is ->
          List.iter (fun i -> 
            Format.fprintf std_formatter "%a\n" pp_instr i
          ) is;
          true))
    ]

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
    (fun _ -> ())
    "help"
;;