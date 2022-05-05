(*
 * SNU 4190.310 Programming Languages
 * K-- to SM5 translator skeleton code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val Sm5.Unit)]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT e1 -> trans e1 @ [Sm5.NOT]
    | K.LETV (x, e1, e2) -> begin
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    end
    | K.LETF (f, x, e1, e2) -> begin
      [Sm5.PUSH (Sm5.Fn(x, [Sm5.BIND f] @ (trans e1))); Sm5.BIND f] @ trans e2 @ [Sm5.UNBIND; Sm5.POP]
    end
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE;] @ trans (K.VAR x)
    | K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE e -> 
      trans e @ [Sm5.MALLOC; Sm5.BIND "_"; Sm5.PUSH (Sm5.Id "_"); Sm5.STORE] @
      [Sm5.PUSH (Sm5.Id "_"); Sm5.LOAD; Sm5.PUT] @
      [Sm5.PUSH (Sm5.Id "_"); Sm5.LOAD] @
      [Sm5.UNBIND; Sm5.POP]
    | K.SEQ (e1, e2) -> trans e1 @ [Sm5.POP] @ trans e2
    | K.ASSIGN (x, e) -> trans e @ [Sm5.PUSH (Sm5.Id x); Sm5.STORE] @ trans (K.VAR x)
    | K.CALLV (f, arg_exp) -> [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @ trans arg_exp @ [Sm5.MALLOC; Sm5.CALL]
    | K.CALLR (f, arg_var) -> [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @ trans (K.VAR arg_var) @ [Sm5.PUSH (Sm5.Id arg_var); Sm5.CALL]
    | K.IF (e_cond, e_true, e_false) -> trans e_cond @ [Sm5.JTR(trans e_true, trans e_false)]
    | K.FOR (id, e1, e2, e_body) -> begin
      trans (K.LETV("_to", e2,
        K.LETF("_recur_for", "_n", 
          K.IF(
            K.LESS(K.VAR "_to", K.VAR "_n"),
            K.UNIT,
            K.SEQ(
              K.ASSIGN(id, K.VAR "_n"),
              K.SEQ(
                e_body,
                K.CALLV("_recur_for", K.ADD(K.VAR "_n", K.NUM 1))
              )
            )
          ),
          K.CALLV("_recur_for", e1)
        )
      ))
    end
    | K.WHILE (e_cond, e_body) -> begin
      trans (K.LETF("_recur_while", "_", 
        K.SEQ(
          e_body,
          K.IF(
            e_cond,
            K.CALLV("_recur_while", K.UNIT),
            K.UNIT
          )
        ),
        K.CALLV("_recur_while", K.UNIT)
      ))
    end
end
