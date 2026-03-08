let affine a b x = (a * x) + b

let main =
  let live = affine 2 7 in
  let _dead = affine 100 1 in
  let _ = print_gc_status () in
  let _ = collect () in
  let _ = print_gc_status () in
  let _dead2 = affine 50 3 in
  let _ = print_gc_status () in
  let _ = collect () in
  let _ = print_gc_status () in
  print_int (live 5)
;;
