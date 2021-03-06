(*
 * SNU 4190.310 Programming Languages 2015 Fall
 *  K- Interpreter Skeleton Code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list      (* call by value *)
  | CALLR of id * id list       (* call by referenece *)
  | RECORD of (id * exp) list   (* record construction *)
  | FIELD of exp * id           (* access record field *)
  | ASSIGN of id * exp          (* assgin to variable *)
  | ASSIGNF of exp * id * exp   (* assign to record field *)
  | READ of id
  | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
  | Num of int
  | Bool of bool
  | Unit
  | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list      (* call by value *)
  | CALLR of id * id list       (* call by referenece *)
  | RECORD of (id * exp) list   (* record construction *)
  | FIELD of exp * id           (* access record field *)
  | ASSIGN of id * exp          (* assgin to variable *)
  | ASSIGNF of exp * id * exp   (* assign to record field *)
  | READ of id
  | WRITE of exp

  type program = exp

  type value =
  | Num of int
  | Bool of bool
  | Unit
  | Record of (id -> Loc.t)
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
      match v with
      | Unit -> ()
      | _ -> raise (Error "TypeError : not unit")

  let value_record v =
      match v with
      | Record r -> r
      | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound loc")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound proc")

  let rec eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | LETF (id, args, body, exp) -> 
      eval mem (Env.bind env id (Proc(args, body, env))) exp
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | NUM n -> (Num n, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | RECORD fields ->
      let rec bind_fields mem vars result = (match vars with
      | (id, p)::tail -> 
        let (v, mem') = eval mem env p in
        let (l, mem'') = Mem.alloc mem' in
        let mem''' = Mem.store mem'' l v in
        bind_fields mem''' tail ((id, l)::result)
      | [] -> (mem, result)
      )
      in
      let (mem, record) = bind_fields mem fields [] in
      let rec find r id = (match r with
      | (field, l)::t -> if id = field then l else find t id
      | [] -> failwith ("cannout find field: "^id)
      )
      in
      (Record (find record), mem)
    | FIELD (exp, id) ->
      let (record, mem') = eval mem env exp in
      let loc = value_record record id in
      let field = Mem.load mem' loc in
      (field, mem')
    | ASSIGNF (r, id, e) ->
      let (record, mem') = eval mem env r in
      let loc = value_record record id in
      let (new_val, mem'') = eval mem' env e in
      let mem''' = Mem.store mem'' loc new_val in
      (new_val, mem''')
    | VAR id ->
      let loc = lookup_env_loc env id in
      let v = Mem.load mem loc in
      (v, mem)
    | SEQ (e1, e2) -> 
      let (_, mem') = eval mem env e1 in
      let (v, mem'') = eval mem' env e2 in
      (v, mem'')
    | ADD (left, right) ->
      let (l, mem') = eval mem env left in
      let (r, mem'') = eval mem' env right in
      let result = value_int l + value_int r in
      (Num result, mem'')
    | MUL (left, right) ->
      let (l, mem') = eval mem env left in
      let (r, mem'') = eval mem' env right in
      let result = value_int l * value_int r in
      (Num result, mem'')
    | DIV (left, right) ->
      let (l, mem') = eval mem env left in
      let (r, mem'') = eval mem' env right in
      let result = value_int l / value_int r in
      (Num result, mem'')
    | SUB (left, right) ->
      let (l, mem') = eval mem env left in
      let (r, mem'') = eval mem' env right in
      let result = value_int l - value_int r in
      (Num result, mem'')
    | CALLV (id, programs) ->
      let (args, body, env') = lookup_env_proc env id in
      let (mem', env'') = bind_args mem env env' args programs in
      eval mem' (Env.bind env'' id (Proc(args, body, env''))) body
    | CALLR (id, refs) ->
      let (args, body, env') = lookup_env_proc env id in
      let (mem', env'') = bind_refs mem env env' args refs in
      eval mem' (Env.bind env'' id (Proc(args, body, env''))) body
    | IF (cond, t, f) -> 
      let (c, mem') = eval mem env cond in
      (match c with
      | Bool(true) -> eval mem' env t
      | Bool(false) -> eval mem' env f
      | _ -> failwith "invalid if")
    | NOT e ->
      let (b, mem') = eval mem env e in
      let r = (match b with
        | Bool(b) -> Bool(not b)
        | _ -> failwith "invalid if")
      in
        (r, mem')
    | EQUAL (left, right) ->
      let (l, mem') = eval mem env left in
      let (r, mem'') = eval mem' env right in
      (match (l, r) with
      | (Num(ln), Num(rn)) -> (Bool(ln = rn), mem'')
      | (Bool(lb), Bool(rb)) -> (Bool(lb = rb), mem'')
      | _ -> failwith "less: arguments are not same type")     
    | LESS (left, right) -> 
      let (l, mem') = eval mem env left in
      let (r, mem'') = eval mem' env right in
      (match (l, r) with
      | (Num(ln), Num(rn)) -> (Bool(ln < rn), mem'')
      | _ -> failwith "less: result is not Num")     
    | WHILE (cond, body) -> 
      let (c, mem') = eval mem env cond in
      if (value_bool c) then (
        let (_, mem'') = eval mem' env body in
        eval mem'' env (WHILE(cond, body))
      ) else
        (c, mem')

  and bind_args mem env fenv args programs = (match (args, programs) with
    | ([], []) -> (mem, fenv)
    | (id::ids, p::ps) ->
      let (v, mem') = eval mem env p in
      let (l, mem'') = Mem.alloc mem' in
      bind_args (Mem.store mem'' l v) env (Env.bind fenv id (Addr l)) ids ps
    | _ -> failwith "not match args length"
  )

  and bind_refs mem env fenv args refs = (match (args, refs) with
  | ([], []) -> (mem, fenv)
  | (id::ids, r::rs) ->
    let loc = lookup_env_loc env r in
    bind_refs mem env (Env.bind fenv id (Addr loc)) ids rs
  | _ -> failwith "not match refs length"
  )

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
