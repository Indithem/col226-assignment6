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
  (* | Ifthenelse of expression * expression * expression *)
  (* | Tuple of expression list *)
  (* | Project of expression * expression *)
  (* | Declaration of string * expression *)
  (* | Function of string * string list * expression list *)
  (* | Function_application of string * expression list *)
  (* | Print of expression *)

  | Var of string
  | Lambda of string * expression 
  | Application of expression * expression
;;
(** Expressions for the Krivine machine *)
type krivine_expression =
  | CONST of inbuilt_const_types
  | OPERATION of inbuilt_operations * krivine_expression * krivine_expression

  | APPLICATION of krivine_expression * krivine_expression (*KrOp*)
  | LOOKUP of string (*KrVar*)
  | LAMBDA_ABSTR of string * krivine_expression (*KrApp*)
;;
  
(** Compiling the expression into a list of opcodes *)
let rec compile e =
  match e with
    | Const i -> CONST i
    | Operation (op, e1, e2) -> OPERATION (op, compile e1, compile e2)
    
    | Var s -> LOOKUP s
    | Lambda (s, e) -> LAMBDA_ABSTR (s, compile e)
    | Application (e1, e2) -> APPLICATION (compile e1, compile e2)
    (* | Ifthenelse (e1, e2, e3) -> (compile e1) @ [IFTHENELSE (compile e2, compile e3)] *)
    (* | Tuple es -> [MAKE_TUPLE (List.map compile es)] *)
    (* | Project (i, e) -> (compile e) @ [PROJECT (compile i)] *)
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
  | CLOSURE of krivine_expression * (closure) StringMap.t

type stack = closure list

exception Krivine_error of string * stack
let rec krivine_machine focus (s:stack) =
  let (expression, environment) = match focus with CLOSURE (ex, en) -> (ex, en) in
  match expression, s with
    | CONST c, _ -> CLOSURE (CONST c, StringMap.empty)
    | OPERATION (op, e1, e2), s ->
      let c1 = krivine_machine (CLOSURE(e1, environment)) s  in
      let c2 = krivine_machine (CLOSURE(e2, environment)) s in
      let res = 
        match c1, c2 with
          | CLOSURE (CONST (Int i1), _), CLOSURE (CONST (Int i2), _) -> 
            (match op with
              | Add -> Int (i1 + i2)
              | Sub -> Int (i1 - i2)
              | Mul -> Int (i1 * i2)
              | Div -> if i2 = 0 then raise (Krivine_error ("Division by zero", (focus::s)))
                else Int (i1 / i2))
          | _ -> raise (Krivine_error ("Invalid operation", focus::s)) in
      CLOSURE (CONST res, StringMap.empty)
    
    (* KrOp *)
    | APPLICATION (e1, e2), s ->
      krivine_machine (CLOSURE(e1, environment)) (CLOSURE(e2, environment)::s)
    (* KrVar *)
    | LOOKUP x, s -> 
      let new_focus = (try StringMap.find x environment with
        | Not_found -> raise (Krivine_error ("Variable not found", focus::s))) in
      krivine_machine new_focus s
    (* KrApp *)
    | LAMBDA_ABSTR (x, e), cl::s ->
      krivine_machine (CLOSURE(e, StringMap.add x cl environment)) s
        
    | _ -> raise (Krivine_error ("Invalid State", focus::s))