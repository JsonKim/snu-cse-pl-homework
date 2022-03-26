exception NOMOVE of string

type item = string
type tree = LEAF of item | NODE of tree list

type zipper = TOP | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
| LOC(t, TOP) -> raise (NOMOVE "left of top")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
| LOC(t, TOP) -> raise (NOMOVE "right of top")
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
| LOC(t, TOP) -> raise (NOMOVE "")
| LOC(t, HAND(left, up, right)) -> LOC(NODE (List.concat [List.rev left; [t]; right]), up)

let goDown loc = match loc with
| LOC(LEAF(_), _) -> raise (NOMOVE "down here")
| LOC(NODE([]), _) -> raise (NOMOVE "empty node")
| LOC(NODE(h::t), up) -> LOC(h, HAND([], up, t))

;;

let empty = LOC (NODE [], TOP)
let loc = LOC (LEAF "*", HAND ([LEAF "c"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"]))

let loc1 = goLeft loc
let loc2 = goUp loc
let loc3 = goRight loc
let loc4 = goLeft (goLeft loc2)
let loc5 = goRight (goDown loc4)
;;

assert (try goLeft empty = empty with NOMOVE _ -> true | _ -> false);
assert (try goRight empty = empty with NOMOVE _ -> true | _ -> false);
assert (try goUp empty = empty with NOMOVE _ -> true | _ -> false);
assert (try goDown empty = empty with NOMOVE _ -> true | _ -> false);

assert (try goDown loc = empty with NOMOVE _ -> true | _ -> false);

assert (loc1 = LOC (LEAF "c", HAND ([], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "*"; LEAF "d"])));
assert (try goLeft loc1 = empty with NOMOVE _ -> true | _ -> false);
assert (goRight loc1 = loc);
assert (try goDown loc1 = empty with NOMOVE _ -> true | _ -> false);

assert (loc2 = LOC (NODE [LEAF "c"; LEAF "*"; LEAF "d"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [])));
assert (goLeft loc2 = LOC (LEAF "+", HAND ([NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [NODE [LEAF "c"; LEAF "*"; LEAF "d"]])));
assert (goLeft (goLeft loc2) = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]])));
assert (try goLeft (goLeft (goLeft loc2)) = empty with NOMOVE _ -> true | _ -> false);
assert (try goRight loc2 = empty with NOMOVE _ -> true | _ -> false);
assert (goUp loc1 = loc2);
assert (goUp loc2 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]], TOP));
assert (try goLeft (goUp loc2) = empty with NOMOVE _ -> true | _ -> false);
assert (try goRight (goUp loc2) = empty with NOMOVE _ -> true | _ -> false);
assert (try goUp (goUp loc2) = empty with NOMOVE _ -> true | _ -> false);
assert (goDown loc2 = loc1);

assert (loc3 = LOC (LEAF "d", HAND ([LEAF "*"; LEAF "c"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [])));
assert (goLeft loc3 = loc);
assert (try goRight loc3 = empty with NOMOVE _ -> true | _ -> false);
assert (goUp loc3 = loc2);
assert (try goDown loc3 = empty with NOMOVE _ -> true | _ -> false);

assert (loc4 = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]])));
assert (loc5 = LOC (LEAF "*", HAND ([LEAF "a"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]), [LEAF "b"])));
;;

let loc1 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"];
                      LEAF "+";
                      NODE [LEAF "c"; LEAF "*"; LEAF "d"]],
                TOP)

let (|>) g f = f g
;;

assert( loc1 |> goDown =
  LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"],
   HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))
);

assert( loc1 |> goDown |> goDown =
  LOC (LEAF "a",
   HAND ([], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]),
    [LEAF "*"; LEAF "b"]))
);

assert( loc1 |> goDown |> goUp |> goDown =
  LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"],
   HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))
);

assert( loc1 |> goDown |> goDown |> goRight =
  LOC (LEAF "*",
   HAND ([LEAF "a"],
    HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]),
    [LEAF "b"]))
);

assert( loc1 |> goDown |> goDown |> goRight |> goLeft |> goRight |> goRight =
  LOC (LEAF "b",
   HAND ([LEAF "*"; LEAF "a"],
    HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]),
    []))
);

assert( loc1 |> goDown |> goRight |> goRight |> goDown |> goRight =
  LOC (LEAF "*",
   HAND ([LEAF "c"],
    HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []),
    [LEAF "d"]))
);

assert (try (loc1 |> goUp |> ignore); false with NOMOVE _ -> true);
;;
