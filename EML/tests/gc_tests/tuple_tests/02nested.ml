let mul2 x = x * 2

let main =
  let t = 1, 2, 3, 4, (5, (6, mul2)), 7, 8, 9 in
  let a, b, c, d, (e, (f, g)), h, i, j = t in
  let _ = print_gc_status () in
  let _ = print_int (g (a + b + c + d + e + f + h + i + j)) in
  let _ = collect () in
  print_gc_status ()
;;
