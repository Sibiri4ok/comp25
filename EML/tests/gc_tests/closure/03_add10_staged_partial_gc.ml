let add10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

let main =
  let c1 = add10 1 2 3 in
  let _ = print_gc_status () in
  let c2 = c1 4 5 in
  let _ = print_gc_status () in
  let _ = collect () in
  let _ = print_gc_status () in
  let c3 = c2 6 7 in
  let _ = collect () in
  let _ = print_gc_status () in
  let c4 = c3 8 9 in
  print_int (c4 10)
;;
