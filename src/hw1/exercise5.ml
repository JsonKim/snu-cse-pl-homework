type nat = ZERO | SUCC of nat

let rec natadd (n1, n2): nat =
  match (n1, n2) with
  | (ZERO, nat) -> nat
  | (nat, ZERO) -> nat
  | (SUCC nat1, SUCC nat2) -> SUCC(SUCC(natadd(nat1, nat2)))

let rec mul (n1,n2) = match n2 with
| 0 -> 0
| 1 -> n1
| _ -> n1 + mul(n1, n2-1)

let rec natmul (n1, n2): nat =
  match n2 with
  | ZERO -> ZERO
  | SUCC ZERO -> n1
  | SUCC nat  -> natadd(n1, natmul(n1, nat))
;;

(* Exercise 5 *)
let one = SUCC ZERO
let three = SUCC (SUCC (SUCC ZERO))
let four = SUCC three
let rec nat_to_int (input: nat): int =
  match input with
  | ZERO -> 0
  | SUCC(i1) -> 1 + (nat_to_int i1)
let rec int_to_nat (input: int): nat =
  match input with
  | 0 -> ZERO
  | _ -> SUCC (int_to_nat (input - 1))
;;

assert (7 = nat_to_int (natadd (three, four)));
assert (0 = nat_to_int (natadd (ZERO, ZERO)));
assert (3 = nat_to_int (natadd (ZERO, three)));
assert (4 = nat_to_int (natadd (four, ZERO)));

assert (12 = nat_to_int (natmul (three, four)));
assert (0 = nat_to_int (natmul (ZERO, three)));
assert (0 = nat_to_int (natmul (four, ZERO)));
assert (0 = nat_to_int (natmul (ZERO, ZERO)));
assert (3 = nat_to_int (natmul (SUCC ZERO, three)));
assert (4 = nat_to_int (natmul (four, SUCC ZERO)));

assert (nat_to_int (natadd (int_to_nat 0, int_to_nat 0)) = 0);
assert (nat_to_int (natadd (int_to_nat 2, int_to_nat 0)) = 2);
assert (nat_to_int (natadd (int_to_nat 0, int_to_nat 3)) = 3);
assert (nat_to_int (natadd (int_to_nat 1, int_to_nat 5)) = 6);
assert (nat_to_int (natadd (int_to_nat 3, int_to_nat 3)) = 6);
assert (nat_to_int (natadd (int_to_nat 12, int_to_nat 7)) = 19);
assert (nat_to_int (natadd (int_to_nat 34, int_to_nat 19)) = 53);

assert (nat_to_int (natmul (int_to_nat 0, int_to_nat 0)) = 0);
assert (nat_to_int (natmul (int_to_nat 2, int_to_nat 0)) = 0);
assert (nat_to_int (natmul (int_to_nat 0, int_to_nat 3)) = 0);
assert (nat_to_int (natmul (int_to_nat 1, int_to_nat 5)) = 5);
assert (nat_to_int (natmul (int_to_nat 3, int_to_nat 3)) = 9);
assert (nat_to_int (natmul (int_to_nat 11, int_to_nat 7)) = 77);
assert (nat_to_int (natmul (int_to_nat 8, int_to_nat 12)) = 96);

assert (natadd (ZERO, ZERO) = ZERO);
assert (natadd (ZERO, (SUCC (SUCC ZERO))) = (SUCC (SUCC ZERO)));
assert (natadd ((SUCC (SUCC ZERO)), (SUCC (SUCC (SUCC ZERO)))) = (SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))));
assert (natmul (ZERO, (SUCC (SUCC ZERO))) = ZERO);
assert (natmul ((SUCC (SUCC ZERO)), (SUCC (SUCC (SUCC ZERO)))) = (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))));
;;
