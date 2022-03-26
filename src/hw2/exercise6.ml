module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end

module IntListQ =
  struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ ((l, r), e): queue = (e :: l, r)
    let deQ (l, r) =
      match r with
      | [] -> (match List.rev l with
        | [] -> raise EMPTY_Q
        | h::t -> (h, ([], t)))
      | h::t -> (h, (l, t))
  end
;;
module ValidIntListQ = (IntListQ : Queue)

let q1 = IntListQ.emptyQ
let q2 = IntListQ.enQ (q1, [1])
let q3 = IntListQ.enQ (q2, [2])
let q4 = IntListQ.enQ (q3, [3])
let x1, q5 = IntListQ.deQ q4
let q6 = IntListQ.enQ (q5, [4])
let q7 = IntListQ.enQ (q6, [5])
let q8 = IntListQ.enQ (q7, [6])
let x2, q9 = IntListQ.deQ q8
let x3, qa = IntListQ.deQ q9
let x4, qb = IntListQ.deQ qa
let x5, qc = IntListQ.deQ qb
let x6, qd = IntListQ.deQ qc
;;

assert (fst (IntListQ.deQ q2) = [1]);
assert (fst (IntListQ.deQ q5) = [2]);
assert (fst (IntListQ.deQ (snd (IntListQ.deQ q5))) = [3]);
assert (x1 = [1]);
assert (x2 = [2]);
assert (x3 = [3]);
assert (x4 = [4]);
assert (x5 = [5]);
assert (x6 = [6]);
assert (qd = q1);
assert (try IntListQ.deQ qd = ([], q1) with IntListQ.EMPTY_Q -> true | _ -> false);
;;

let q1 = IntListQ.emptyQ
let q2 = IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(q1, [1]), [2; 3]), [4; 5; 6])
let e1, q3 = IntListQ.deQ q2
;;

assert (q1 = ([], []));
assert (q2 = ([[4; 5; 6]; [2; 3]; [1]], []));
assert (e1 = [1]);
assert (q3 = ([], [[2; 3]; [4; 5; 6]]));
;;
