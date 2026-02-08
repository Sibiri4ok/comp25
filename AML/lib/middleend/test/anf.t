(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../../../bin/AML.exe --dump-anf ./manytests/typed/010fac_anf.ml
  let rec fac =
    fun n ->
      let t_0 = n <= 1 in
      if t_0 then
        1
      else
        let t_1 = n - 1 in
        let n1 = t_1 in
        let t_2 = fac n1 in
        let m = t_2 in
        n * m
  
  let main =
    fac 4

  $ ../../../bin/AML.exe --dump-anf ./manytests/typed/010faccps_ll.ml
  let id =
    fun x ->
      x
  
  let fresh_1 =
    fun n ->
      fun k ->
        fun p ->
          let t_1 = p * n in
          k t_1
  
  let rec fac_cps =
    fun n ->
      fun k ->
        let t_4 = n = 1 in
        if t_4 then
          k 1
        else
          let t_6 = n - 1 in
          let t_7 = fresh_1 n k in
          fac_cps t_6 t_7
  
  let main =
    let t_10 = fac_cps 4 id in
    let t_11 = print_int t_10 in
    let t_12 = t_11 in
    0
  $ ../../../bin/AML.exe --dump-anf ./manytests/typed/010fibcps_ll.ml
  let id =
    fun x ->
      x
  
  let fresh_2 =
    fun p1 ->
      fun k ->
        fun p2 ->
          let t_1 = p1 + p2 in
          k t_1
  
  let fresh_1 =
    fun n ->
      fun k ->
        fun fib ->
          fun p1 ->
            let t_4 = n - 2 in
            let t_5 = fresh_2 p1 k in
            fib t_4 t_5
  
  let rec fib =
    fun n ->
      fun k ->
        let t_8 = n < 2 in
        if t_8 then
          k n
        else
          let t_10 = n - 1 in
          let t_11 = fresh_1 n k fib in
          fib t_10 t_11
  
  let main =
    let t_14 = fib 6 id in
    let t_15 = print_int t_14 in
    let z = t_15 in
    0

=================== custom (before cc + ll) ===================
  $ cat >many_args_pa.ml <<EOF
  > let wrap f = if 1 = 1 then f else f
  > 
  > let test3 a b c =
  > let a = print_int a in
  > let b = print_int b in
  > let c = print_int c in
  > 0
  > 
  > let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j
  > 
  > let main =
  > let rez =
  >     (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
  >        1000000000)
  > in
  > let () = print_int rez in
  > let temp2 = wrap test3 1 10 100 in
  > 0
  > EOF
  $ ../../../bin/AML.exe --dump-anf many_args_pa.ml
  let wrap =
    fun f ->
      let t_0 = 1 = 1 in
      if t_0 then
        f
      else
        f
  
  let test3 =
    fun a ->
      fun b ->
        fun c ->
          let t_2 = print_int a in
          let a = t_2 in
          let t_3 = print_int b in
          let b = t_3 in
          let t_4 = print_int c in
          let c = t_4 in
          0
  
  let test10 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                fun g ->
                  fun h ->
                    fun i ->
                      fun j ->
                        let t_6 = a + b in
                        let t_7 = t_6 + c in
                        let t_8 = t_7 + d in
                        let t_9 = t_8 + e in
                        let t_10 = t_9 + f in
                        let t_11 = t_10 + g in
                        let t_12 = t_11 + h in
                        let t_13 = t_12 + i in
                        t_13 + j
  
  let main =
    let t_16 = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
    let rez = t_16 in
    let t_17 = print_int rez in
    let () = t_17 in
    let t_18 = wrap test3 1 10 100 in
    let temp2 = t_18 in
    0

=================== custom (after cc, before ll) ===================
  $ cat >many_args_pa.ml <<EOF
  > let wrap f = if 1 = 1 then f else f
  > 
  > let test3 a b c =
  > let a = print_int a in
  > let b = print_int b in
  > let c = print_int c in
  > 0
  > 
  > let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j
  > 
  > let main =
  > let rez =
  >     (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
  >        1000000000)
  > in
  > let () = print_int rez in
  > let temp2 = wrap test3 1 10 100 in
  > 0
  > EOF
  $ ../../../bin/AML.exe --dump-cc-anf many_args_pa.ml
  let wrap =
    fun f ->
      let t_0 = 1 = 1 in
      if t_0 then
        f
      else
        f
  
  let test3 =
    fun a ->
      let f_cc_0 = fun a ->
        fun b ->
          let f_cc_1 = fun a ->
            fun b ->
              fun c ->
                let t_2 = print_int a in
                let a = t_2 in
                let t_3 = print_int b in
                let b = t_3 in
                let t_4 = print_int c in
                let c = t_4 in
                0 in
          f_cc_1 a b in
      f_cc_0 a
  
  let test10 =
    fun a ->
      let f_cc_2 = fun a ->
        fun b ->
          let f_cc_3 = fun a ->
            fun b ->
              fun c ->
                let f_cc_4 = fun a ->
                  fun b ->
                    fun c ->
                      fun d ->
                        let f_cc_5 = fun a ->
                          fun b ->
                            fun c ->
                              fun d ->
                                fun e ->
                                  let f_cc_6 = fun a ->
                                    fun b ->
                                      fun c ->
                                        fun d ->
                                          fun e ->
                                            fun f ->
                                              let f_cc_7 = fun a ->
                                                fun b ->
                                                  fun c ->
                                                    fun d ->
                                                      fun e ->
                                                        fun f ->
                                                          fun g ->
                                                            let f_cc_8 = fun a ->
                                                              fun b ->
                                                                fun c ->
                                                                  fun d ->
                                                                    fun e ->
                                                                      fun f ->
                                                                        fun g ->
                                                                          fun h ->
                                                                            let f_cc_9 = fun a ->
                                                                              fun b ->
                                                                                fun c ->
                                                                                  fun d ->
                                                                                    fun e ->
                                                                                      fun f ->
                                                                                        fun g ->
                                                                                          fun h ->
                                                                                            fun i ->
                                                                                              let f_cc_10 = fun a ->
                                                                                                fun b ->
                                                                                                  fun c ->
                                                                                                    fun d ->
                                                                                                      fun e ->
                                                                                                        fun f ->
                                                                                                          fun g ->
                                                                                                            fun h ->
                                                                                                              fun i ->
                                                                                                                fun j ->
                                                                                                                  let t_6 = a + b in
                                                                                                                  let t_7 = t_6 + c in
                                                                                                                  let t_8 = t_7 + d in
                                                                                                                  let t_9 = t_8 + e in
                                                                                                                  let t_10 = t_9 + f in
                                                                                                                  let t_11 = t_10 + g in
                                                                                                                  let t_12 = t_11 + h in
                                                                                                                  let t_13 = t_12 + i in
                                                                                                                  t_13 + j in
                                                                                              f_cc_10 a b c d e f g h i in
                                                                            f_cc_9 a b c d e f g h in
                                                            f_cc_8 a b c d e f g in
                                              f_cc_7 a b c d e f in
                                  f_cc_6 a b c d e in
                        f_cc_5 a b c d in
                f_cc_4 a b c in
          f_cc_3 a b in
      f_cc_2 a
  
  let main =
    let t_16 = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
    let rez = t_16 in
    let t_17 = print_int rez in
    let () = t_17 in
    let t_18 = wrap test3 1 10 100 in
    let temp2 = t_18 in
    0

=================== custom (after cc + ll) ===================
  $ cat >many_args_pa.ml <<EOF
  > let wrap f = if 1 = 1 then f else f
  > 
  > let test3 a b c =
  > let a = print_int a in
  > let b = print_int b in
  > let c = print_int c in
  > 0
  > 
  > let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j
  > 
  > let main =
  > let rez =
  >     (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
  >        1000000000)
  > in
  > let () = print_int rez in
  > let temp2 = wrap test3 1 10 100 in
  > 0
  > EOF
  $ ../../../bin/AML.exe --dump-cc-ll-anf many_args_pa.ml
  let llf_0 =
    fun f ->
      let t_0 = 1 = 1 in
      if t_0 then
        f
      else
        f
  
  let llf_1 =
    fun a ->
      fun b ->
        fun c ->
          let t_2 = print_int a in
          let a = t_2 in
          let t_3 = print_int b in
          let b = t_3 in
          let t_4 = print_int c in
          let c = t_4 in
          0
  
  let llf_2 =
    fun a ->
      fun b ->
        let f_cc_1 = llf_1 in
        f_cc_1 a b
  
  let llf_3 =
    fun a ->
      let f_cc_0 = llf_2 in
      f_cc_0 a
  
  let llf_4 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                fun g ->
                  fun h ->
                    fun i ->
                      fun j ->
                        let t_6 = a + b in
                        let t_7 = t_6 + c in
                        let t_8 = t_7 + d in
                        let t_9 = t_8 + e in
                        let t_10 = t_9 + f in
                        let t_11 = t_10 + g in
                        let t_12 = t_11 + h in
                        let t_13 = t_12 + i in
                        t_13 + j
  
  let llf_5 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                fun g ->
                  fun h ->
                    fun i ->
                      let f_cc_10 = llf_4 in
                      f_cc_10 a b c d e f g h i
  
  let llf_6 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                fun g ->
                  fun h ->
                    let f_cc_9 = llf_5 in
                    f_cc_9 a b c d e f g h
  
  let llf_7 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                fun g ->
                  let f_cc_8 = llf_6 in
                  f_cc_8 a b c d e f g
  
  let llf_8 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                let f_cc_7 = llf_7 in
                f_cc_7 a b c d e f
  
  let llf_9 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              let f_cc_6 = llf_8 in
              f_cc_6 a b c d e
  
  let llf_10 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            let f_cc_5 = llf_9 in
            f_cc_5 a b c d
  
  let llf_11 =
    fun a ->
      fun b ->
        fun c ->
          let f_cc_4 = llf_10 in
          f_cc_4 a b c
  
  let llf_12 =
    fun a ->
      fun b ->
        let f_cc_3 = llf_11 in
        f_cc_3 a b
  
  let llf_13 =
    fun a ->
      let f_cc_2 = llf_12 in
      f_cc_2 a
  
  let wrap =
    llf_0
  
  let test3 =
    llf_3
  
  let test10 =
    llf_13
  
  let main =
    let t_16 = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
    let rez = t_16 in
    let t_17 = print_int rez in
    let () = t_17 in
    let t_18 = wrap test3 1 10 100 in
    let temp2 = t_18 in
    0


=================== faccps_ll (before cc + ll) ===================
  $ ../../../bin/AML.exe --dump-anf ./manytests/typed/010faccps_ll.ml
  let id =
    fun x ->
      x
  
  let fresh_1 =
    fun n ->
      fun k ->
        fun p ->
          let t_1 = p * n in
          k t_1
  
  let rec fac_cps =
    fun n ->
      fun k ->
        let t_4 = n = 1 in
        if t_4 then
          k 1
        else
          let t_6 = n - 1 in
          let t_7 = fresh_1 n k in
          fac_cps t_6 t_7
  
  let main =
    let t_10 = fac_cps 4 id in
    let t_11 = print_int t_10 in
    let t_12 = t_11 in
    0




=================== faccps_ll (after cc, before ll) ===================
  $ ../../../bin/AML.exe --dump-cc-anf ./manytests/typed/010faccps_ll.ml
  let id =
    fun x ->
      x
  
  let fresh_1 =
    fun n ->
      let f_cc_0 = fun n ->
        fun k ->
          let f_cc_1 = fun k ->
            fun n ->
              fun p ->
                let t_1 = p * n in
                k t_1 in
          f_cc_1 k n in
      f_cc_0 n
  
  let fac_cps_cc_2 =
    fun fac_cps ->
      fun fresh_1 ->
        fun n ->
          let f_cc_3 = fun fac_cps ->
            fun fresh_1 ->
              fun n ->
                fun k ->
                  let t_4 = n = 1 in
                  if t_4 then
                    k 1
                  else
                    let t_6 = n - 1 in
                    let t_7 = fresh_1 n k in
                    fac_cps t_6 t_7 in
          f_cc_3 fac_cps fresh_1 n
  
  let rec fac_cps =
    fun n ->
      fac_cps_cc_2 fac_cps fresh_1 n
  
  let main =
    let t_10 = fac_cps 4 id in
    let t_11 = print_int t_10 in
    let t_12 = t_11 in
    0



=================== faccps_ll (after cc + ll) ===================
  $ ../../../bin/AML.exe --dump-cc-ll-anf ./manytests/typed/010faccps_ll.ml
  let llf_0 =
    fun x ->
      x
  
  let llf_1 =
    fun k ->
      fun n ->
        fun p ->
          let t_1 = p * n in
          k t_1
  
  let llf_2 =
    fun n ->
      fun k ->
        let f_cc_1 = llf_1 in
        f_cc_1 k n
  
  let llf_3 =
    fun n ->
      let f_cc_0 = llf_2 in
      f_cc_0 n
  
  let llf_4 =
    fun fac_cps ->
      fun fresh_1 ->
        fun n ->
          fun k ->
            let t_4 = n = 1 in
            if t_4 then
              k 1
            else
              let t_6 = n - 1 in
              let t_7 = fresh_1 n k in
              fac_cps t_6 t_7
  
  let llf_5 =
    fun fac_cps ->
      fun fresh_1 ->
        fun n ->
          let f_cc_3 = llf_4 in
          f_cc_3 fac_cps fresh_1 n
  
  let llf_6 =
    fun n ->
      fac_cps_cc_2 fac_cps fresh_1 n
  
  let id =
    llf_0
  
  let fresh_1 =
    llf_3
  
  let fac_cps_cc_2 =
    llf_5
  
  let rec fac_cps =
    llf_6
  
  let main =
    let t_10 = fac_cps 4 id in
    let t_11 = print_int t_10 in
    let t_12 = t_11 in
    0





=================== faccps (before cc + ll) ===================
  $ ../../../bin/AML.exe --dump-anf ./manytests/typed/012faccps.ml
  let rec fac =
    fun n ->
      fun k ->
        let t_0 = n < 2 in
        if t_0 then
          k 1
        else
          let t_2 = n - 1 in
          let t_5 = fun a ->
            let t_3 = a * n in
            k t_3 in
          fac t_2 t_5
  
  let main =
    let t_8 = fun x ->
      x in
    let t_9 = fac 6 t_8 in
    print_int t_9

=================== faccps (after cc, before ll) ===================
  $ ../../../bin/AML.exe --dump-cc-anf ./manytests/typed/012faccps.ml
  let rec fac =
    fun n ->
      let f_cc_0 = fun fac ->
        fun n ->
          fun k ->
            let t_0 = n < 2 in
            if t_0 then
              k 1
            else
              let t_2 = n - 1 in
              let t_5_cc_1 = fun k ->
                fun n ->
                  fun a ->
                    let t_3 = a * n in
                    k t_3 in
              let t_5 = t_5_cc_1 k n in
              fac t_2 t_5 in
      f_cc_0 fac n
  
  let main =
    let t_8 = fun x ->
      x in
    let t_9 = fac 6 t_8 in
    print_int t_9

=================== faccps (after cc + ll) ===================
  $ ../../../bin/AML.exe --dump-cc-ll-anf ./manytests/typed/012faccps.ml
  let llf_0 =
    fun k ->
      fun n ->
        fun a ->
          let t_3 = a * n in
          k t_3
  
  let llf_1 =
    fun fac ->
      fun n ->
        fun k ->
          let t_0 = n < 2 in
          if t_0 then
            k 1
          else
            let t_2 = n - 1 in
            let t_5_cc_1 = llf_0 in
            let t_5 = t_5_cc_1 k n in
            fac t_2 t_5
  
  let llf_2 =
    fun n ->
      let f_cc_0 = llf_1 in
      f_cc_0 fac n
  
  let llf_3 =
    fun x ->
      x
  
  let rec fac =
    llf_2
  
  let main =
    let t_8 = llf_3 in
    let t_9 = fac 6 t_8 in
    print_int t_9

==== 4 task fact no cc no ll ====
  $ ../../../bin/AML.exe --dump-anf ./manytests/typed/012faccps.ml
  let rec fac =
    fun n ->
      fun k ->
        let t_0 = n < 2 in
        if t_0 then
          k 1
        else
          let t_2 = n - 1 in
          let t_5 = fun a ->
            let t_3 = a * n in
            k t_3 in
          fac t_2 t_5
  
  let main =
    let t_8 = fun x ->
      x in
    let t_9 = fac 6 t_8 in
    print_int t_9

==== 4 task fact no ll ====
  $ ../../../bin/AML.exe --dump-cc-anf ./manytests/typed/012faccps.ml
  let rec fac =
    fun n ->
      let f_cc_0 = fun fac ->
        fun n ->
          fun k ->
            let t_0 = n < 2 in
            if t_0 then
              k 1
            else
              let t_2 = n - 1 in
              let t_5_cc_1 = fun k ->
                fun n ->
                  fun a ->
                    let t_3 = a * n in
                    k t_3 in
              let t_5 = t_5_cc_1 k n in
              fac t_2 t_5 in
      f_cc_0 fac n
  
  let main =
    let t_8 = fun x ->
      x in
    let t_9 = fac 6 t_8 in
    print_int t_9
==== 4 task fact ====
  $ ../../../bin/AML.exe --dump-cc-ll-anf ./manytests/typed/012faccps.ml
  let llf_0 =
    fun k ->
      fun n ->
        fun a ->
          let t_3 = a * n in
          k t_3
  
  let llf_1 =
    fun fac ->
      fun n ->
        fun k ->
          let t_0 = n < 2 in
          if t_0 then
            k 1
          else
            let t_2 = n - 1 in
            let t_5_cc_1 = llf_0 in
            let t_5 = t_5_cc_1 k n in
            fac t_2 t_5
  
  let llf_2 =
    fun n ->
      let f_cc_0 = llf_1 in
      f_cc_0 fac n
  
  let llf_3 =
    fun x ->
      x
  
  let rec fac =
    llf_2
  
  let main =
    let t_8 = llf_3 in
    let t_9 = fac 6 t_8 in
    print_int t_9

==== 4 task fib no cc no ll ====
  $ ../../../bin/AML.exe --dump-anf ./manytests/typed/012fibcps.ml
  let rec fib =
    fun n ->
      fun k ->
        let t_0 = n < 2 in
        if t_0 then
          k n
        else
          let t_2 = n - 1 in
          let t_8 = fun a ->
            let t_3 = n - 2 in
            let t_6 = fun b ->
              let t_4 = a + b in
              k t_4 in
            fib t_3 t_6 in
          fib t_2 t_8
  
  let main =
    let t_11 = fun x ->
      x in
    let t_12 = fib 6 t_11 in
    print_int t_12

==== 4 task fib no ll ====
  $ ../../../bin/AML.exe --dump-cc-anf ./manytests/typed/012fibcps.ml
  let rec fib =
    fun n ->
      let f_cc_0 = fun fib ->
        fun n ->
          fun k ->
            let t_0 = n < 2 in
            if t_0 then
              k n
            else
              let t_2 = n - 1 in
              let t_8_cc_1 = fun fib ->
                fun k ->
                  fun n ->
                    fun a ->
                      let t_3 = n - 2 in
                      let t_6_cc_2 = fun a ->
                        fun k ->
                          fun b ->
                            let t_4 = a + b in
                            k t_4 in
                      let t_6 = t_6_cc_2 a k in
                      fib t_3 t_6 in
              let t_8 = t_8_cc_1 fib k n in
              fib t_2 t_8 in
      f_cc_0 fib n
  
  let main =
    let t_11 = fun x ->
      x in
    let t_12 = fib 6 t_11 in
    print_int t_12
==== 4 task fib ====
  $ ../../../bin/AML.exe --dump-cc-ll-anf ./manytests/typed/012fibcps.ml
  let llf_0 =
    fun a ->
      fun k ->
        fun b ->
          let t_4 = a + b in
          k t_4
  
  let llf_1 =
    fun fib ->
      fun k ->
        fun n ->
          fun a ->
            let t_3 = n - 2 in
            let t_6_cc_2 = llf_0 in
            let t_6 = t_6_cc_2 a k in
            fib t_3 t_6
  
  let llf_2 =
    fun fib ->
      fun n ->
        fun k ->
          let t_0 = n < 2 in
          if t_0 then
            k n
          else
            let t_2 = n - 1 in
            let t_8_cc_1 = llf_1 in
            let t_8 = t_8_cc_1 fib k n in
            fib t_2 t_8
  
  let llf_3 =
    fun n ->
      let f_cc_0 = llf_2 in
      f_cc_0 fib n
  
  let llf_4 =
    fun x ->
      x
  
  let rec fib =
    llf_3
  
  let main =
    let t_11 = llf_4 in
    let t_12 = fib 6 t_11 in
    print_int t_12

==== 4 task fib ====
  $ ../../../bin/AML.exe --dump-cc-ll-anf ./manytests/typed/010faccps_ll.ml
  let llf_0 =
    fun x ->
      x
  
  let llf_1 =
    fun k ->
      fun n ->
        fun p ->
          let t_1 = p * n in
          k t_1
  
  let llf_2 =
    fun n ->
      fun k ->
        let f_cc_1 = llf_1 in
        f_cc_1 k n
  
  let llf_3 =
    fun n ->
      let f_cc_0 = llf_2 in
      f_cc_0 n
  
  let llf_4 =
    fun fac_cps ->
      fun fresh_1 ->
        fun n ->
          fun k ->
            let t_4 = n = 1 in
            if t_4 then
              k 1
            else
              let t_6 = n - 1 in
              let t_7 = fresh_1 n k in
              fac_cps t_6 t_7
  
  let llf_5 =
    fun fac_cps ->
      fun fresh_1 ->
        fun n ->
          let f_cc_3 = llf_4 in
          f_cc_3 fac_cps fresh_1 n
  
  let llf_6 =
    fun n ->
      fac_cps_cc_2 fac_cps fresh_1 n
  
  let id =
    llf_0
  
  let fresh_1 =
    llf_3
  
  let fac_cps_cc_2 =
    llf_5
  
  let rec fac_cps =
    llf_6
  
  let main =
    let t_10 = fac_cps 4 id in
    let t_11 = print_int t_10 in
    let t_12 = t_11 in
    0

  $ ../../../bin/AML.exe --dump-cc-anf  ./manytests/typed/004manyargs.ml 
  let wrap =
    fun f ->
      let t_0 = 1 = 1 in
      if t_0 then
        f
      else
        f
  
  let test3 =
    fun a ->
      let f_cc_0 = fun a ->
        fun b ->
          let f_cc_1 = fun a ->
            fun b ->
              fun c ->
                let t_2 = print_int a in
                let a = t_2 in
                let t_3 = print_int b in
                let b = t_3 in
                let t_4 = print_int c in
                let c = t_4 in
                0 in
          f_cc_1 a b in
      f_cc_0 a
  
  let test10 =
    fun a ->
      let f_cc_2 = fun a ->
        fun b ->
          let f_cc_3 = fun a ->
            fun b ->
              fun c ->
                let f_cc_4 = fun a ->
                  fun b ->
                    fun c ->
                      fun d ->
                        let f_cc_5 = fun a ->
                          fun b ->
                            fun c ->
                              fun d ->
                                fun e ->
                                  let f_cc_6 = fun a ->
                                    fun b ->
                                      fun c ->
                                        fun d ->
                                          fun e ->
                                            fun f ->
                                              let f_cc_7 = fun a ->
                                                fun b ->
                                                  fun c ->
                                                    fun d ->
                                                      fun e ->
                                                        fun f ->
                                                          fun g ->
                                                            let f_cc_8 = fun a ->
                                                              fun b ->
                                                                fun c ->
                                                                  fun d ->
                                                                    fun e ->
                                                                      fun f ->
                                                                        fun g ->
                                                                          fun h ->
                                                                            let f_cc_9 = fun a ->
                                                                              fun b ->
                                                                                fun c ->
                                                                                  fun d ->
                                                                                    fun e ->
                                                                                      fun f ->
                                                                                        fun g ->
                                                                                          fun h ->
                                                                                            fun i ->
                                                                                              let f_cc_10 = fun a ->
                                                                                                fun b ->
                                                                                                  fun c ->
                                                                                                    fun d ->
                                                                                                      fun e ->
                                                                                                        fun f ->
                                                                                                          fun g ->
                                                                                                            fun h ->
                                                                                                              fun i ->
                                                                                                                fun j ->
                                                                                                                  let t_6 = a + b in
                                                                                                                  let t_7 = t_6 + c in
                                                                                                                  let t_8 = t_7 + d in
                                                                                                                  let t_9 = t_8 + e in
                                                                                                                  let t_10 = t_9 + f in
                                                                                                                  let t_11 = t_10 + g in
                                                                                                                  let t_12 = t_11 + h in
                                                                                                                  let t_13 = t_12 + i in
                                                                                                                  t_13 + j in
                                                                                              f_cc_10 a b c d e f g h i in
                                                                            f_cc_9 a b c d e f g h in
                                                            f_cc_8 a b c d e f g in
                                              f_cc_7 a b c d e f in
                                  f_cc_6 a b c d e in
                        f_cc_5 a b c d in
                f_cc_4 a b c in
          f_cc_3 a b in
      f_cc_2 a
  
  let main =
    let t_16 = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
    let rez = t_16 in
    let t_17 = print_int rez in
    let () = t_17 in
    let t_18 = wrap test3 1 10 100 in
    let temp2 = t_18 in
    0

  $ ../../../bin/AML.exe --dump-cc-ll-anf  ./manytests/typed/004manyargs.ml 
  let llf_0 =
    fun f ->
      let t_0 = 1 = 1 in
      if t_0 then
        f
      else
        f
  
  let llf_1 =
    fun a ->
      fun b ->
        fun c ->
          let t_2 = print_int a in
          let a = t_2 in
          let t_3 = print_int b in
          let b = t_3 in
          let t_4 = print_int c in
          let c = t_4 in
          0
  
  let llf_2 =
    fun a ->
      fun b ->
        let f_cc_1 = llf_1 in
        f_cc_1 a b
  
  let llf_3 =
    fun a ->
      let f_cc_0 = llf_2 in
      f_cc_0 a
  
  let llf_4 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                fun g ->
                  fun h ->
                    fun i ->
                      fun j ->
                        let t_6 = a + b in
                        let t_7 = t_6 + c in
                        let t_8 = t_7 + d in
                        let t_9 = t_8 + e in
                        let t_10 = t_9 + f in
                        let t_11 = t_10 + g in
                        let t_12 = t_11 + h in
                        let t_13 = t_12 + i in
                        t_13 + j
  
  let llf_5 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                fun g ->
                  fun h ->
                    fun i ->
                      let f_cc_10 = llf_4 in
                      f_cc_10 a b c d e f g h i
  
  let llf_6 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                fun g ->
                  fun h ->
                    let f_cc_9 = llf_5 in
                    f_cc_9 a b c d e f g h
  
  let llf_7 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                fun g ->
                  let f_cc_8 = llf_6 in
                  f_cc_8 a b c d e f g
  
  let llf_8 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              fun f ->
                let f_cc_7 = llf_7 in
                f_cc_7 a b c d e f
  
  let llf_9 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            fun e ->
              let f_cc_6 = llf_8 in
              f_cc_6 a b c d e
  
  let llf_10 =
    fun a ->
      fun b ->
        fun c ->
          fun d ->
            let f_cc_5 = llf_9 in
            f_cc_5 a b c d
  
  let llf_11 =
    fun a ->
      fun b ->
        fun c ->
          let f_cc_4 = llf_10 in
          f_cc_4 a b c
  
  let llf_12 =
    fun a ->
      fun b ->
        let f_cc_3 = llf_11 in
        f_cc_3 a b
  
  let llf_13 =
    fun a ->
      let f_cc_2 = llf_12 in
      f_cc_2 a
  
  let wrap =
    llf_0
  
  let test3 =
    llf_3
  
  let test10 =
    llf_13
  
  let main =
    let t_16 = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
    let rez = t_16 in
    let t_17 = print_int rez in
    let () = t_17 in
    let t_18 = wrap test3 1 10 100 in
    let temp2 = t_18 in
    0
