let inc x = x + 1

let main =
  let t = 41, (2, inc) in
  let n, (_, f) = t in
  let _ = print_gc_status () in
  let _ = print_int (f n) in
  let _ = collect () in
  print_gc_status ()
;;
