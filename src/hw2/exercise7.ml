module Zexpr =
  struct
    exception Error of string
    type id = string
    type expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
    | MULT of expr * expr
    | DIVIDE of expr * expr
    | MAX of expr list
    | VAR of id
    | LET of id * expr * expr

    type value = int

    module Dic = Map.Make(String)
    type environment = value Dic.t

    let emptyEnv = Dic.empty
    let eval (env, exp) =
      let rec calc env exp = match exp with
      | NUM n -> n
      | PLUS(e1, e2) -> calc env e1 + calc env e2
      | MINUS(e1, e2) -> calc env e1 - calc env e2
      | MULT(e1, e2) -> calc env e1 * calc env e2
      | DIVIDE(e1, e2) -> calc env e1 / calc env e2
      | MAX exps -> (
        match List.map (calc env) exps with
        | [] -> 0
        | values -> List.fold_left max min_int values
      )
      | VAR(id) -> (
        try Dic.find id env with
        | Not_found -> raise (Error "not found variable")
      )
      | LET(id, e1, e2) -> calc (Dic.add id (calc env e1) env) e2
      in calc env exp

    let print_value = print_int
  end

;;

open Zexpr

let test (expr: expr) = print_value (eval (emptyEnv, expr)); print_string "\n"
let ex_test (expr: expr) = assert (try test expr; false with | _ -> true)

let xpx = PLUS (VAR "x", VAR "x")
let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS (Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x"))))
let xmx = MULT (VAR "x", VAR "x")
let lyxx = LET ("y", xpx, MULT (NUM (-1), VAR "y"))
let zyyx = LET ("z", xpx,
  LET ("y", MAX [NUM (-3); NUM (-2); NUM (-1)],
    LET ("y", PLUS (VAR "y", VAR "z"),
      LET ("x", xmx, DIVIDE (MULT (VAR "x", VAR "y"), NUM 2)))))
;;

test (NUM 7);
test (LET ("x", NUM 12, VAR "x"));
test (LET ("x", NUM 10, MINUS (NUM 8, VAR "x")));
test (LET ("x", NUM 10, PLUS (NUM (-8), VAR "x")));
test (LET ("x", NUM 2, DIVIDE (NUM 8, VAR "x")));

test (MAX []);
test (MAX [NUM min_int]);
test (LET ("x", NUM (-1), MAX [VAR "x"; NUM (-2)]));
test (LET ("x", NUM 2, MAX [NUM 1; NUM (-3); MAX []; lyxx; xpx]));

test (LET ("y", NUM 1, PLUS (LET ("y", NUM 2, PLUS (VAR "y", VAR "y")), VAR "y")));
test (LET ("x", NUM 1, PLUS (LET ("y", NUM 2, PLUS (VAR "x", VAR "y")), VAR "x")));
test (LET ("x", NUM 4, MULT (LET ("x", xpx, xpx), LET ("x", NUM 2, LET ("x", xpx, xpx)))));
test (LET ("x", NUM 2, MULT (LET ("x", NUM 1, MAX [NUM 0; VAR "x"; NUM 3]), VAR "x")));
test (LET ("x", MAX [NUM 3; NUM 8; NUM 5; NUM 2; NUM 8; NUM 7; NUM 6], LET ("y", PLUS (xmx, xpx), PLUS (LET ("x", NUM 1, lyxx), zyyx))));

test (NUM 1);
test e1;
test (MAX []);
test (MAX [NUM (-1)]);

ex_test (LET ("y", NUM 2, VAR "x"));
ex_test (LET ("x", NUM 1, PLUS (LET ("y", NUM 2, PLUS (VAR "x", VAR "y")), VAR "y")));
ex_test (LET ("x", MAX [], (LET ("y", NUM 128, DIVIDE (VAR "y", VAR "x")))));
ex_test (LET ("x", MAX [], (LET ("y", NUM 0, DIVIDE (VAR "y", VAR "x")))));
