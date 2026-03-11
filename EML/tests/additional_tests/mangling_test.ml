let rec field n = if n <= 1 then 1 else n * field (n - 1)

let main =
  let () = print_int (field 4) in
  0
;;
