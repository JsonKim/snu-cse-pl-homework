type formula =
| TRUE
| FALSE
| NOT of formula
| ANDALSO of  formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr

and expr =
| NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec expr e =
  match e with
  | NUM(n) -> n
  | PLUS(e1, e2) -> expr(e1) + expr(e2)
  | MINUS(e1, e2) -> expr(e1) - expr(e2)

let rec eval formula =
  match formula with
  | TRUE -> true
  | FALSE -> false
  | NOT(f) -> not (eval f)
  | ANDALSO(f1, f2) ->  eval(f1) && eval(f2)
  | ORELSE(f1, f2) -> eval(f1) || eval(f2)
  | IMPLY(f1, f2) -> (match (eval(f1), eval(f2)) with
    | (true, true) -> true
    | (false, _) -> true
    | (_, _) -> false)
  | LESS(f1, f2) -> expr(f1) < expr(f2)
;;

(* Exercise 4 *)
let f1 = TRUE
let f2 = FALSE
let f3 = NOT f1
let f4 = ANDALSO (NOT f2, ANDALSO (f3, f1))
let f5 = ORELSE (ORELSE (f3, f1), f4)
let f6 = IMPLY (f4, f5)
let f7 = IMPLY (f5, ORELSE (f4, FALSE))
let f8 = ORELSE (IMPLY (NOT f6, f2), ANDALSO (ORELSE (f3, NOT f4), NOT f7))
let f9 = LESS (NUM 1, NUM 2)
let fa = LESS (PLUS (NUM 1, NUM 2), MINUS (NUM 0, NUM 121))
let fb =
  LESS
    (MINUS
      (PLUS (NUM 5, MINUS (NUM 1, NUM 21)),
       MINUS (NUM 0, NUM 100)), NUM 2)
;;

assert (true = eval TRUE);
assert (false = eval FALSE);
assert (false = eval (NOT TRUE));
assert (true = eval (NOT FALSE));
assert (true = eval (ANDALSO (TRUE, TRUE)));
assert (false = eval (ANDALSO (TRUE, FALSE)));
assert (false = eval (ANDALSO (FALSE, TRUE)));
assert (false = eval (ANDALSO (FALSE, FALSE)));
assert (true = eval (ORELSE (TRUE, TRUE)));
assert (true = eval (ORELSE (TRUE, FALSE)));
assert (true = eval (ORELSE (FALSE, TRUE)));
assert (false = eval (ORELSE (FALSE, FALSE)));
assert (false = eval (IMPLY (TRUE, FALSE)));
assert (true = eval (IMPLY (TRUE, TRUE)));
assert (true = eval (IMPLY (FALSE, TRUE)));
assert (true = eval (IMPLY (FALSE, FALSE)));
assert (true = eval (LESS (NUM 3, NUM 5)));
assert (false = eval (LESS (NUM 3, NUM 3)));
assert (false = eval (LESS (NUM 3, NUM 1)));
assert (false = eval (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1))));
assert (true = eval (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13)))));
assert (eval f1 = true);
assert (eval f2 = false);
assert (eval f3 = false);
assert (eval f4 = false);
assert (eval f5 = true);
assert (eval f6 = true);
assert (eval f7 = false);
assert (eval f8 = true);
assert (eval f9 = true);
assert (eval fa = false);
assert (eval fb = false);

assert (eval (IMPLY(FALSE, FALSE)) = true);
assert (eval (ANDALSO (ORELSE(TRUE, FALSE), NOT (IMPLY(TRUE, FALSE)))) = true);
assert ((eval (ANDALSO (TRUE, TRUE))) && (not (eval (ANDALSO (TRUE, FALSE)))) && (not (eval (ANDALSO (FALSE, TRUE)))) && (not (eval (ANDALSO (FALSE, FALSE)))) = true);
assert ((eval (ORELSE (TRUE, TRUE))) && (eval (ORELSE (TRUE, FALSE))) && (eval (ORELSE (FALSE, TRUE))) && (not (eval (ORELSE (FALSE, FALSE)))) = true);
assert ((eval (IMPLY (TRUE, TRUE))) && (not (eval (IMPLY (TRUE, FALSE)))) && (eval (IMPLY (FALSE, TRUE))) && (eval (IMPLY (FALSE, FALSE))) = true);
assert (eval (LESS (NUM 3, NUM 5)) = true);
assert (eval (LESS (PLUS (NUM 3, NUM 5), PLUS (NUM 1, NUM 2))) = false);
assert (eval (LESS (MINUS (NUM 3, NUM 5), MINUS (NUM 1, NUM 2))) = true);
assert (eval (ORELSE (LESS (PLUS (MINUS (NUM 3, NUM 2), NUM 9), NUM 10), FALSE)) = false);
assert (eval (IMPLY(LESS (NUM 1, NUM 0), ANDALSO(TRUE, ORELSE(NOT TRUE, LESS(NUM 2, NUM 1))))) = true);
;;
