type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2



let rec crazy2add ((v1, v2): crazy2 * crazy2): crazy2 =
  match (v1, v2) with
  | (ONE n1, MONE n2) -> ZERO(crazy2add(n1, n2))
  | (MONE n1, ONE n2) -> ZERO(crazy2add(n1, n2))
  | (ZERO n1, ZERO n2) -> ZERO(crazy2add(n1, n2))
  | (ONE n1, ZERO n2) -> ONE(crazy2add(n1, n2))
  | (ZERO n1, ONE n2) -> ONE(crazy2add(n1, n2))
  | (MONE n1, ZERO n2) -> MONE(crazy2add(n1, n2))
  | (ZERO n1, MONE n2) -> MONE(crazy2add(n1, n2))
  | (ONE n1, ONE n2) -> ZERO(crazy2add(ONE NIL, (crazy2add(n1, n2))))
  | (MONE n1, MONE n2) -> ZERO(crazy2add(MONE NIL, (crazy2add(n1, n2))))
  | (NIL, _) -> v2
  | (_, NIL) -> v1
;;

let zero = ZERO NIL
let one = MONE (ONE (ZERO NIL))
let four = ZERO (ZERO (MONE (ONE NIL)))
let m_five = ONE (MONE (ONE (MONE NIL)))
let m_one = (ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(MONE NIL))))))))))))))))))))))))))))))

let rec crazy2val (input: crazy2): int =
  match input with
  | NIL -> 0
  | ZERO rest -> 2*(crazy2val rest)
  | ONE  rest -> 1 + 2*(crazy2val rest)
  | MONE rest -> -1 + 2*(crazy2val rest)
;;

assert (  0 = crazy2val (crazy2add (zero, zero)));
assert (  1 = crazy2val (crazy2add (zero, one)));
assert (  4 = crazy2val (crazy2add (zero, four)));
assert ( -5 = crazy2val (crazy2add (zero, m_five)));
assert ( -1 = crazy2val (crazy2add (zero, m_one)));
assert (  1 = crazy2val (crazy2add (one, zero)));
assert (  2 = crazy2val (crazy2add (one, one)));
assert (  5 = crazy2val (crazy2add (one, four)));
assert ( -4 = crazy2val (crazy2add (one, m_five)));
assert (  0 = crazy2val (crazy2add (one, m_one)));
assert (  4 = crazy2val (crazy2add (four, zero)));
assert (  5 = crazy2val (crazy2add (four, one)));
assert (  8 = crazy2val (crazy2add (four, four)));
assert ( -1 = crazy2val (crazy2add (four, m_five)));
assert (  3 = crazy2val (crazy2add (four, m_one)));
assert ( -5 = crazy2val (crazy2add (m_five, zero)));
assert ( -4 = crazy2val (crazy2add (m_five, one)));
assert ( -1 = crazy2val (crazy2add (m_five, four)));
assert (-10 = crazy2val (crazy2add (m_five, m_five)));
assert ( -6 = crazy2val (crazy2add (m_five, m_one)));
assert ( -1 = crazy2val (crazy2add (m_one, zero)));
assert (  0 = crazy2val (crazy2add (m_one, one)));
assert (  3 = crazy2val (crazy2add (m_one, four)));
assert ( -6 = crazy2val (crazy2add (m_one, m_five)));
assert ( -2 = crazy2val (crazy2add (m_one, m_one)));
