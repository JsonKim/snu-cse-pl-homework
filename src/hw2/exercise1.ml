type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val (crazy2: crazy2): int = 
  match crazy2 with
  | NIL -> 0
  | ZERO n -> 2 * (crazy2val n)
  | ONE n -> (2 * (crazy2val n)) + 1
  | MONE n -> (2 * (crazy2val n)) - 1
;;

let zero = ZERO NIL
let one = MONE (ONE (ZERO NIL))
let four = ZERO (ZERO (MONE (ONE NIL)))
let m_five = ONE (MONE (ONE (MONE NIL)))
let m_one = (ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(MONE NIL))))))))))))))))))))))))))))))
;;

assert ( 0 = crazy2val zero);
assert ( 1 = crazy2val one);
assert ( 4 = crazy2val four);
assert (-5 = crazy2val m_five);
assert (-1 = crazy2val m_one);
;;