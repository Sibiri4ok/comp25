let add5 a b c d e = a + b + c + d + e

let main =
  let p1 = add5 1 2 in
  let _ = print_gc_status () in
  let p2 = p1 3 in
  let _ = print_gc_status () in
  let _ = collect () in
  let _ = print_gc_status () in
  let p3 = p2 4 in
  let _ = collect () in
  let _ = print_gc_status () in
  print_int (p3 5)
;;
