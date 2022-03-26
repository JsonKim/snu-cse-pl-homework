type metro =
| STATION of name
| AREA of name * metro
| CONNECT of metro * metro

and name = string

let rec checkMetro metro: bool =
  match metro with
  | AREA(a, STATION(s)) -> a = s
  | AREA(a1, AREA(a2, s)) -> checkMetro(AREA(a1, s)) || checkMetro(AREA(a2, s))
  | AREA(a1, CONNECT(STATION(s1), STATION(s2))) -> a1 = s1 || a1 = s2
  | AREA(a1, CONNECT(STATION(s1), AREA(_, a2))) -> a1 = s1 && checkMetro(AREA(a1, a2))
  | AREA(a1, CONNECT(AREA(_, a2), STATION(s1))) -> a1 = s1 && checkMetro(AREA(a1, a2))
  | CONNECT(c1, c2) -> checkMetro(c1) && checkMetro(c2)
  | _ -> false

let rec checkMetro metro: bool =
  let rec check name metro =
    match metro with
    | STATION(s) -> name = s
    | AREA(a, m) -> check name m || check a m
    | CONNECT(c1 ,c2) -> check name c1 || check name c2
  in
  match metro with
  | AREA(a, m) -> check a m
  | CONNECT(c1, c2) -> checkMetro c1 && checkMetro c2
  | _ -> false

let checkMetro (input: metro): bool =
  let rec check (input: metro) (rules: string list): bool =
    match input with
    (* STATION 을 만날때 까지 이름을 누적해서 전달하고 그 안에 STATION이 있는지 확인 *)
    | STATION name -> List.exists ((=) name) rules
    | AREA (name, m0) -> check m0 (name::rules)
    | CONNECT (m0, m1) -> (check m0 rules) && (check m1 rules)
  in
  check input []
;;

assert (checkMetro (AREA("a", STATION "a") ));
assert (checkMetro (AREA("a", AREA("a", STATION "a")) ));
assert (checkMetro (AREA("a", AREA("b", STATION "a")) ));
assert (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))) ));
assert (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))) ));
assert (checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a")))) ));
assert (not (checkMetro (AREA("a", STATION "b") )));

assert (not (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))) )));
assert (not (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))) )));
assert (not (checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c")))) )));
;;
