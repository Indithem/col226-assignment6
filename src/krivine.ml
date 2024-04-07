type inbuilt_operations = 
  | Add
  | Sub
  | Mul
  | Div

type inbuilt_const_types = 
  | Int of int
  | Bool of bool
  | String of string

  (**The abract syntax tree of the assignment *)
type expression = 
  | Const of inbuilt_const_types
  | Operation of inbuilt_operations * expression * expression
  (* Ifthenelse of bool * exp1 * exp2 *)
  | Ifthenelse of expression * expression * expression
  | Tuple of expression list
  (* Project of index from tuple *)
  | Project of expression * expression
  (* | Declaration of string * expression *)
  (* | Function of string * string list * expression list *)
  (* | Function_application of string * expression list *)
  (* | Print of expression *)

  | Var of string
  | Lambda of string * expression 
  | Application of expression * expression
;;
  
(** Compiling the expression into a reduced expression
    The only purpose of the compile function is to desugar the inputs *)
let rec compile (e:expression) =
  match e with
    | _ -> e
    (* | Ifthenelse (e1, e2, e3) -> (compile e1) @ [IFTHENELSE (compile e2, compile e3)] *)
    (* | Declaration (s, e) -> (compile e) @ [ASSIGN s] *)
    (* | Function (x, vars, e) -> 
      let rec lambda_expr vars=
        match vars with 
          | [] -> Lambda ("", e)
          | [x] -> Lambda (x, e)
          | x::xs -> Lambda (x, [lambda_expr xs])
      in
      compile (Declaration(x, lambda_expr (List.rev vars))) *)
    (* | Function_application (f, es) ->
      [LOOKUP f] @ 
      (match es with | [] -> [PUSH (Int 0)] | _ -> List.concat_map compile es) 
      @ [APPLY_CLOSURE] *)
    (* | Print e -> (compile e) @ [PRINT] *)
;;

module StringMap = Map.Make(String)

type closure = 
  (* krivine_expression * (closure) StringMap.t 
     This definition could not be made due to cyclic dependency*)
  | CLOSURE of expression * (closure) StringMap.t

type stack = closure list

exception Krivine_error of string * stack
let rec krivine_machine focus (s:stack) =
  let (expression, environment) = match focus with CLOSURE (ex, en) -> (ex, en) in
  match expression, s with
    | Const c, _ -> CLOSURE (Const c, StringMap.empty)
    | Tuple es, s -> 
      let eval_expr e =
        match krivine_machine (CLOSURE(e, environment)) s with
          CLOSURE (e, _) -> e      
      in
      CLOSURE (
        Tuple (List.map eval_expr es)
        , StringMap.empty
      )
    | Operation (op, e1, e2), s ->
      let c1 = krivine_machine (CLOSURE(e1, environment)) s  in
      let c2 = krivine_machine (CLOSURE(e2, environment)) s in
      let res = 
        match c1, c2 with
          | CLOSURE (Const (Int i1), _), CLOSURE (Const (Int i2), _) -> 
            (match op with
              | Add -> Int (i1 + i2)
              | Sub -> Int (i1 - i2)
              | Mul -> Int (i1 * i2)
              | Div -> if i2 = 0 then raise (Krivine_error ("Division by zero", (focus::s)))
                else Int (i1 / i2))
          | _ -> raise (Krivine_error ("Invalid operation", focus::s)) in
      CLOSURE (Const res, StringMap.empty)

    | Project (e1, Tuple(e2)), s ->
      let c1 = krivine_machine (CLOSURE(e1, environment)) s in
      let index = match c1 with CLOSURE (Const (Int i), _) -> i | _ -> raise (Krivine_error ("Invalid index", focus::s)) in
      let expr = (try
        (List.nth e2 index)
      with 
        | Failure _ -> raise (Krivine_error ("Index out of bounds", focus::s))
        | Invalid_argument _ -> raise (Krivine_error ("Invalid index", focus::s))) in
      krivine_machine (CLOSURE(expr, environment)) s

    | Ifthenelse (e1, e2, e3), s->
      let c1 = krivine_machine (CLOSURE(e1, environment)) s in
      let res = match c1 with
        | CLOSURE (Const (Bool b), _) -> if b then e2 else e3
        | _ -> raise (Krivine_error ("Invalid condition", focus::s)) in
      krivine_machine (CLOSURE(res, environment)) s
    
    (* KrOp *)
    | Application (e1, e2), s ->
      krivine_machine (CLOSURE(e1, environment)) (CLOSURE(e2, environment)::s)
    (* KrVar *)
    | Var x, s -> 
      let new_focus = (try StringMap.find x environment with
        | Not_found -> raise (Krivine_error ("Variable not found", focus::s))) in
      krivine_machine new_focus s
    (* KrApp *)
    | Lambda (x, e), cl::s ->
      krivine_machine (CLOSURE(e, StringMap.add x cl environment)) s
        
    | _ -> raise (Krivine_error ("Invalid State", focus::s))