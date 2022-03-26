let rec sigma (a, b, f) =
  if a < b then f(a) + sigma(a+1, b, f)
  else if a == b then f(a)
  else 0

let foo = fun n -> n * 2
let bar = fun n -> n * n
;;

assert (385 = sigma (1, 10, fun x -> x*x));
assert (0 = sigma (3, 1, fun x -> x*x));
assert (27 = sigma (3, 3, fun x -> x*x*x));
assert (385 = sigma (-10, -1, fun x -> x*x));
assert (sigma (3, 1, foo) = 0);
assert (sigma (4, 2, bar) = 0);
assert (sigma (8, 8, foo) = 16);
assert (sigma (3, 4, bar) = 25);
assert (sigma (1, 10, foo) = 110);
assert (sigma (1, 10, bar) = 5 * 7 * 11);
assert (sigma (5, 10, foo) = 90);
assert (sigma (1, 100, foo) = 10100);

assert (sigma (10, 10, fun x -> x) = 10);
assert (sigma (11, 10, fun x -> x) = 0);
assert (sigma (10, 5, fun x -> x) = 0);
assert (sigma (1, 10, fun x -> if x mod 2 = 0 then 1 else 0 ) = 5);
assert (sigma (1, 10, fun x -> x * x) = 385);
;;
