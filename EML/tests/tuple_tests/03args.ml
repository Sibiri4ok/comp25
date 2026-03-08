let add3 x = x + 3

let main =
  let pack = 10, 20, 30, 40, 50, (60, (70, add3)), 80, 90, 100, 110, 120, 130, 140 in
  let a, b, c, d, e, (f, (g, h)), i, j, k, l, m, n, o = pack in
  let base = a + b + c + d + e + f + g + i + j + k + l + m + n + o in
  let _ = print_gc_status () in
  let _ = print_int (h base) in
  let _ = collect () in
  print_gc_status ()
;;
