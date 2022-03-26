type exp =
| X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreeVariable

let rec sigma (a: int) (b: int) (func: float -> float): float =
  if a == b then func (float_of_int a)
  else if a > b then 0.0
  else func (float_of_int a) +. sigma (a+1) b func

let rec integral (a: float) (b: float) (func: float -> float): float =
  let step = 0.1 in
  if a > b then -. integral b a func
  else if a +. step -. 0.00000001 > b then 0.0
  else step *. func a +. integral (a+.step) b func

let galculator exp =
  let rec eval exp x =
    match exp with
    | X -> (match x with
      | Some n -> n
      | None -> raise FreeVariable)
    | INT(n) -> float_of_int n
    | REAL(n) -> n
    | ADD(e1, e2) -> eval e1 x +. eval e2 x
    | SUB(e1, e2) -> eval e1 x -. eval e2 x
    | MUL(e1, e2) -> eval e1 x *. eval e2 x
    | DIV(e1, e2) -> eval e1 x /. eval e2 x
    | SIGMA(min, max, f) ->
        let a = eval min x in
        let b = eval max x in
        sigma (int_of_float a) (int_of_float b) (fun h -> eval f (Some h))
    | INTEGRAL(min, max, f) ->
        let a = eval min x in
        let b = eval max x in
        integral a b (fun h -> eval f (Some h))
    in eval exp None
  ;;

let n1 = INT 3
let n2 = REAL 1.2
let n3 = INT (-2)
let n4 = REAL 0.8
let x1 = ADD (X, INT 1)
let x2 = MUL (X, (MUL (INT 2, X)))
let x3 = SUB (MUL (X, X), INT 1)
let s1 = SIGMA (INT 0, INT 1, X)
let s2 = SIGMA (INT 0, X, MUL (MUL (X, X), INT 3))
let s3 = SIGMA (s1, INT 10, s2)

let check (sigma: float) (a: float) (b: float): bool = a -. b < sigma && b -. a < sigma
let check_tight = check 0.00001
let check_loose = check 0.005
;;

(* Arithmatic *)
assert (check_tight (galculator n1) 3.0);
assert (check_tight (galculator n2) 1.2);
assert (check_tight (galculator n3) (-2.0));
assert (check_tight (galculator n4) 0.8);
assert (check_tight (galculator (ADD (n1, n2))) 4.2);
assert (check_tight (galculator (ADD (ADD (n1, n2), n3))) 2.2);
assert (check_tight (galculator (ADD (ADD (n1, n2), n4))) 5.0);
assert (check_tight (galculator (SUB (n1, n2))) 1.8);
assert (check_tight (galculator (SUB (n4, n3))) 2.8);
assert (check_tight (galculator (SUB (SUB (n4, n3), n3))) 4.8);
assert (check_tight (galculator (MUL (n1, n2))) 3.6);
assert (check_tight (galculator (MUL (ADD (n3, n4), n2))) (-1.44));
assert (check_tight (galculator (MUL (n1, (SUB (INT 0, n2))))) (-3.6));
assert (check_tight (galculator (DIV (n1, n2))) 2.5);
assert (check_tight (galculator (DIV (n4, n3))) (-0.4));
assert (try check_tight (galculator X) 123.0 with FreeVariable -> true | _ -> false);

(* Sigma *)
assert (galculator (SIGMA (INT 10, INT 1, X)) = 0.0);
assert (check_tight (galculator (SIGMA (INT 1, INT 10, REAL 0.5))) 5.0);
assert (check_tight (galculator (SIGMA (INT 1, INT 10, X))) 55.0);
assert (check_tight (galculator (SIGMA (REAL 1.0, INT 100, x1))) 5150.0);
assert (check_tight (galculator (SIGMA (REAL 1.0, REAL 10.1, x2))) 770.0);
assert (check_tight (galculator (SIGMA (INT 4, INT 12, MUL ((SUB (X, REAL 1.0)), x1)))) 627.0);
assert (check_tight (galculator (SIGMA (INT 4, INT 12, x3))) 627.0);
assert (check_tight (galculator s3) 3630.0);
assert (
  check_tight (galculator
  (SIGMA
    (SUB (INT 3, REAL 1.0),
    SIGMA (INT 1, INT 3, X),
    SIGMA (X, ADD (X, X), SUB (MUL (INT 2, MUL (X, X)), MUL (REAL 3.0, X))))))
  2015.0);
assert (
  check_tight (galculator
  (SIGMA (SIGMA (INT 2, INT 1, X), INT 10,
    (SIGMA (SIGMA (INT (-1), INT 1, X), X,
      (SIGMA (INT 0, X, MUL (X, X))))))))
  3289.0);
assert (try check_tight (galculator (SIGMA (INT 0, X, X))) 0.0 with FreeVariable -> true | _ -> false);

(* Integral *)
assert (check_loose (galculator (INTEGRAL (INT 2, REAL 2.05, ADD (X, X)))) 0.0);
assert (check_loose (galculator (INTEGRAL (REAL (-2.0), REAL (-2.05), DIV (X, X)))) 0.0);
assert (check_loose (galculator (INTEGRAL (INT 0, INT 2, REAL 0.5))) 1.0);
assert (check_loose (galculator (INTEGRAL (INT 2, INT 0, REAL (-1.0)))) 2.0);
assert (check_loose (galculator (INTEGRAL (INT 0, INT 2, MUL (X, INT 2)))) 3.8);
assert (check_loose (galculator (INTEGRAL (REAL 1.55, REAL 1.37, X))) (-0.137));
assert (check_loose (galculator (INTEGRAL (INT 2, INT 0, MUL (X, X)))) (-2.47));
assert (check_loose (galculator (INTEGRAL (REAL 0.1, INT 1, DIV (INT 100, X)))) 282.896);
assert (check_loose (galculator (INTEGRAL (REAL 10.0, REAL 1.0, SUB(MUL(X, X), INT 1)))) (-319.065));
assert (check_loose (galculator (INTEGRAL (INT 1, INT (-1), MUL (MUL (X, X), X)))) 0.1);
assert (check_loose (galculator (INTEGRAL (INT 1, INT 0, MUL (INT 3, MUL (X, X))))) (-0.855));
assert (try check_loose (galculator (INTEGRAL (INT 0, MUL (INT 1, X), X))) 0.0 with FreeVariable -> true | _ -> false);
assert (
  try check_loose
    (galculator
      (INTEGRAL
        (ADD (X, REAL 1.0),
        SIGMA (REAL 1.0, REAL 10.0, SUB (MUL(X, X), INT 1)),
        SUB(MUL(X, X), X))))
    0.0 with FreeVariable -> true | _ -> false);
assert (
  try check_loose
    (galculator
      (SIGMA
        (INTEGRAL (INT 0, MUL (INT 1, INT 0), X),
        INTEGRAL (SUB (INT 1, MUL (INT 2, X)), INT 30, REAL 10.5),
        MUL (X, X))))
    0.0 with FreeVariable -> true | _ -> false);

(* etc *)
assert (try check_tight (galculator X) nan with _ -> true);
assert (try check_tight (galculator (INTEGRAL (ADD (X, REAL 1.), SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X))))
  nan with _ -> true);

assert (check_tight (galculator (SIGMA (INT 1, INT 10, SUB(MUL(X, X), INT 1)))) 375.0);
assert (check_tight (galculator (SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)))) 375.0);
assert (check_loose (galculator (INTEGRAL (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)))) 319.064999999);
assert (check_loose (galculator (INTEGRAL (REAL 10., REAL 1., SUB(MUL(X, X), INT 1)))) (-319.06499999));
(*
assert (check_loose (galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), INT 1)))) 17556346.1239999793);
assert (check_loose (galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)))) 17486504.2639999);
*)
