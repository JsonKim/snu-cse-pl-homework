
let rec merge (xs, ys) =
  match (xs, ys) with
  | (xs, []) -> xs
  | ([], ys) -> ys
  | (x :: xs, y :: ys) ->
      if x > y then
        x :: (merge (xs, (y :: ys)))
      else
        y :: (merge ((x :: xs), ys))

(* Exercise 1 *)
let l0 = []
let l1 = [5; 3; 1]
let l2 = [4; 2; 0]
let l3 = [10; 7; 6]
let l4 = [11; 9; 8]
(*
;;

assert ([10; 10; 4; 3; 2; 1] = (merge ([10; 4; 2], [10; 3; 1])));
assert ([10; 3; 2; -2; -3; -10] = (merge ([3; -2; -10], [10; 2; -3])));
assert (merge (l0, l0) = l0);
assert (merge (l1, l0) = l1);
assert (merge (l0, l2) = l2);
assert (merge (l1, l2) = [5; 4; 3; 2; 1; 0]);
assert (merge (l2, l1) = [5; 4; 3; 2; 1; 0]);
assert (merge (l3, l4) = [11; 10; 9; 8; 7; 6]);
assert (merge (l4, l3) = [11; 10; 9; 8; 7; 6]);
assert (merge (l1, l4) = [11; 9; 8; 5; 3; 1]);
assert (merge (merge (l1, l2), l4) = [11; 9; 8; 5; 4; 3; 2; 1; 0]);
assert (merge (merge (l1, l2), merge (l4, l3)) = [11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0]);

assert (merge ([], [5;4;3;2;1]) = [5;4;3;2;1]);
assert (merge ([10;8;6;4;2], []) = [10;8;6;4;2]);
assert (merge ([3;2;1], [6;5;4]) = [6;5;4;3;2;1]);
assert (merge ([5;3;1], [4;2]) = [5;4;3;2;1]);
assert (merge ([10;2;1], [10;4;3]) = [10;10;4;3;2;1]);
*)

let _ = 
  assert ([10; 10; 4; 3; 2; 1] = (merge ([10; 4; 2], [10; 3; 1])));
  assert ([10; 3; 2; -2; -3; -10] = (merge ([3; -2; -10], [10; 2; -3])));
  assert (merge (l0, l0) = l0)

let _ = 
  let _ = assert ([10; 10; 4; 3; 2; 1] = (merge ([10; 4; 2], [10; 3; 1]))) in
  assert ([10; 3; 2; -2; -3; -10] = (merge ([3; -2; -10], [10; 2; -3])));
  let _ = assert (merge (l0, l0) = l0) in
  assert (merge (l1, l0) = l1)

let buggy_baz x =
  if x < 0
  then print_string "low\n"
  else let message = "high\n" in
    print_string message;
  print_string "ok\n"

let buggy_baz2 x =
  if x < 0
  then print_string "low\n"
  else
    let _ =
      let
        message = "high\n"
      in
        print_string message
    in
      print_string "ok\n"

let buggy_baz3 x =
  if x < 0
  then print_string "low\n"
  else (let message = "high\n" in
    print_string message);
  print_string "ok\n"

let buggy_baz4 x =
  let _ = 
    if x < 0
    then print_string "low\n"
    else let message = "high\n" in
      print_string message
  in
    print_string "ok\n"
    
(* a; b는 let _ = a in b, 여기서 a는 unit이 되는 expression *)
let foo x =
  print_string "ok\n"; x + 3

let foo x =
  let _ = print_string "ok\n" in x + 3

let foo x =
  print_string "ok1\n";
  print_string "ok2\n";
  x + 3

let foo x =
  let _ = print_string "ok1\n" in
  let _ = print_string "ok2\n" in
  x + 3
