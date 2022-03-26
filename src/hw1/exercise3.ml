let rec iter (n, f) =
  if n == 1 then f 
  else if n == 0 then fun x -> x
  else fun x -> f (iter (n-1, f) x)

let f1 = fun x -> 2 + x
let f2 = fun x -> fun y -> x * y
;;

assert (20 = (iter (10, (fun x->x+2))) 0);
assert (100 = (iter (100, (fun x->x+1))) 0)

(*
assert (20 = (iter (10, (fun x->x+2))) 0);
assert (100 = (iter (100, (fun x->x+1))) 0);
assert (0 = (iter (10, (fun x->x))) 0);
assert ((iter (10, f1)) 3 = 23);
assert ((iter (2, f1)) 121 = f1 (f1 121));
assert ((iter (3, f1)) 177 = f1 (f1 (f1 177)));
assert ((iter (4, f2 1)) 44 = 44);
assert (((iter (2, f2 7)) 4 = f2 7 (f2 7 4)));

assert (iter(10, fun x -> x + 3) 100 = 130);
assert (iter(0, fun x -> x + 3) 200 = 200);
assert (iter (3, List.tl) [1;2;3;4;5;6] = [4;5;6]);
assert (iter (4, (fun s -> s ^ s)) "a" = "aaaaaaaaaaaaaaaa");
assert (iter (5, fun (x,y,z) -> (z, x, y)) (1,2,3) = (2, 3, 1))
*)
