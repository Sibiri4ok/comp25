let rec fac n k =
  if n < 2
  then k 1
  else fac (n - 1) (fun a -> k (a * n))

let main = print_int (fac 6 (fun x -> x))
